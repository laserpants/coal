{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CLI.Command.Install (installCommand, installProject) where

import CLI.Error (CLIError (..))
import CLI.Git (gitCheckoutCommit, gitCloneRepo, gitLsRemoteVersions)
import CLI.Git.Commit (GitCommit (..))
import CLI.Git.Repo (GitRepo (..))
import Control.Monad.Except
import Control.Monad.State
import Data.Aeson.Encode.Pretty
import Data.ByteString (toStrict)
import qualified Data.ByteString as ByteString
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.SemVer.Constraint (Constraint (CAny))
import qualified Data.SemVer.Constraint as SemVerConstraint
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Extras (Name, Over)
import Package.Dependency (PackageDependency (..))
import Package.Error (PackageError (..))
import Package.Lock (PackageLock (..))
import Package.Lock.Spec
import Package.Manifest
import Package.Version (AvailableVersion (..), PackageConstraint (..), PackageVersion (..))
import System.Directory

data InstallState = InstallState
  { visited :: Set (Name, GitCommit)
  , lockEntries :: Map Name LockSpec
  }
  deriving (Show, Eq)

overVisited :: Over InstallState (Set (Name, GitCommit))
overVisited fn InstallState{..} = InstallState{visited = fn visited, ..}

overLockEntries :: Over InstallState (Map Name LockSpec)
overLockEntries fn InstallState{..} = InstallState{lockEntries = fn lockEntries, ..}

{-# INLINE initialInstallState #-}
initialInstallState :: InstallState
initialInstallState = InstallState mempty mempty

addVisited :: (Name, GitCommit) -> StateT InstallState (ExceptT CLIError IO) ()
addVisited pkg = modify (overVisited (Set.insert pkg))

addLockEntry :: Name -> LockSpec -> StateT InstallState (ExceptT CLIError IO) ()
addLockEntry name spec = modify (overLockEntries (Map.insert name spec))

installPackage :: Name -> PackageVersion -> GitRepo -> GitCommit -> StateT InstallState (ExceptT CLIError IO) ()
installPackage name version repo commit = do
  InstallState{..} <- get
  unless ((name, commit) `elem` visited) $ do
    addVisited (name, commit)
    let dir = basePath name commit
    exists <- liftIO $ doesDirectoryExist dir

    unless exists $ lift $ do
      gitCloneRepo repo dir
      gitCheckoutCommit commit dir

    addLockEntry name LockSpec{version = version, source = repo, commit = commit}
    PackageManifest{dependencies = deps} <- lift (withExceptT EPackageError (loadManifest name commit))
    installDependencies (fromMaybe mempty deps)

installDependencies :: Map Text PackageDependency -> StateT InstallState (ExceptT CLIError IO) ()
installDependencies deps =
  forM_ (Map.toList deps) $
    \(pkgName, PackageDependency{git = repo, ..}) -> do
      versions <- lift $ gitLsRemoteVersions repo
      case pickVersionHash version versions of
        Nothing -> do
          throwError (EPackageError err)
         where
          err =
            ENoPackageVersionMatch
              pkgName
              (fromMaybe (PackageConstraint CAny) version)
              (availableVersion <$> versions)
        Just (AvailableVersion pkgVersion commit) ->
          installPackage pkgName pkgVersion repo commit

constraintSatisfies :: PackageConstraint -> AvailableVersion -> Bool
constraintSatisfies (PackageConstraint constraint) (AvailableVersion{availableVersion = PackageVersion version}) =
  SemVerConstraint.satisfies version constraint

pickVersionHash :: Maybe PackageConstraint -> [AvailableVersion] -> Maybe AvailableVersion
pickVersionHash Nothing (v : _) = Just v
pickVersionHash (Just constraint) versions = find (constraintSatisfies constraint) versions
pickVersionHash _ _ = Nothing

installProject :: ExceptT CLIError IO ()
installProject = do
  res <- liftIO $ runExceptT loadProjectManifest
  case res of
    Left err ->
      throwError (EPackageError err)
    Right PackageManifest{..} -> do
      InstallState{..} <-
        flip execStateT initialInstallState $
          installDependencies (fromMaybe mempty dependencies)

      liftIO $ do
        ByteString.writeFile "coal.lock.json" (toStrict (encodePretty (PackageLock lockEntries)))
        putStrLn "Writing coal.lock.json"

installCommand :: ExceptT CLIError IO ()
installCommand = installProject
