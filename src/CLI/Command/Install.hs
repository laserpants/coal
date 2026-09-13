{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CLI.Command.Install (installCommand, installProject) where

import CLI.Error (CLIError (..))
import CLI.Git (gitCheckoutCommit, gitCloneRepo, gitLsRemoteVersions)
import CLI.Git.Commit (GitCommit (..))
import CLI.Git.Repo (GitRepo (..))
import CLI.Options.InstallCmd (InstallCmdOptions (..))
import Coal.Compiler.Terminal (TerminalCapabilities, sanitizeForTerminal)
import Control.Monad (unless, when)
import Control.Monad.Except
import Control.Monad.State
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString (toStrict)
import qualified Data.ByteString as ByteString
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.SemVer (toText)
import Data.SemVer.Constraint (Constraint (CAny))
import qualified Data.SemVer.Constraint as SemVerConstraint
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Extras (Name, Over, forM_)
import Package.Dependency (PackageDependency (..))
import Package.Error (PackageError (..))
import Package.Lock (PackageLock (..))
import Package.Lock.Spec (LockSpec (..))
import Package.Manifest
import Package.Version (AvailableVersion (..), PackageConstraint (..), PackageVersion (..))
import System.Directory (doesDirectoryExist)
import System.IO (hPutStrLn, stderr)
import TextShow (showt)

data InstallState = InstallState
  { visited :: Set (Name, GitCommit)
  , lockEntries :: Map Name LockSpec
  , caps :: TerminalCapabilities
  }
  deriving (Show, Eq)

overVisited :: Over InstallState (Set (Name, GitCommit))
overVisited fn InstallState{..} = InstallState{visited = fn visited, ..}

overLockEntries :: Over InstallState (Map Name LockSpec)
overLockEntries fn InstallState{..} = InstallState{lockEntries = fn lockEntries, ..}

{-# INLINE initialInstallState #-}
initialInstallState :: TerminalCapabilities -> InstallState
initialInstallState = InstallState mempty mempty

addVisited :: (Name, GitCommit) -> StateT InstallState (ExceptT CLIError IO) ()
addVisited pkg = modify (overVisited (Set.insert pkg))

addLockEntry :: Name -> LockSpec -> StateT InstallState (ExceptT CLIError IO) ()
addLockEntry name spec = modify (overLockEntries (Map.insert name spec))

{- | Print a progress line to stderr, degrading gracefully to ASCII on
terminals that don't support Unicode (e.g. when output is piped).
-}
announce :: TerminalCapabilities -> Text -> IO ()
announce caps msg =
  hPutStrLn stderr (Text.unpack (sanitizeForTerminal caps ("• " <> msg)))

{- | 'announce' lifted into the install state monad, reading the terminal
capabilities from the install state.
-}
progress :: Text -> StateT InstallState (ExceptT CLIError IO) ()
progress msg = do
  InstallState{caps = c} <- get
  liftIO $ announce c msg

installPackage :: Name -> PackageVersion -> GitRepo -> GitCommit -> StateT InstallState (ExceptT CLIError IO) ()
installPackage name version repo commit = do
  InstallState{..} <- get
  unless ((name, commit) `elem` visited) $ do
    addVisited (name, commit)
    let dir = basePath name commit
    exists <- liftIO $ doesDirectoryExist dir

    if exists
      then progress ("Already installed: " <> describe name version commit)
      else do
        progress ("Cloning " <> name <> " from " <> repoUrl repo <> "...")
        lift (gitCloneRepo repo dir)
        progress ("Checking out " <> describe name version commit <> "...")
        lift (gitCheckoutCommit commit dir)
        progress ("Installed " <> describe name version commit)

    addLockEntry name LockSpec{version = version, source = repo, commit = commit}
    PackageManifest{dependencies = deps} <- lift (withExceptT EPackageError (loadManifest name commit))
    installDependencies (fromMaybe mempty deps)

installDependencies :: Map Text PackageDependency -> StateT InstallState (ExceptT CLIError IO) ()
installDependencies deps =
  forM_ (Map.toList deps) $
    \(pkgName, PackageDependency{git = repo, ..}) -> do
      progress ("Resolving versions for " <> pkgName <> " (" <> repoUrl repo <> ")...")
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
        Just (AvailableVersion pkgVersion commit) -> do
          progress ("Resolved " <> describe pkgName pkgVersion commit)
          installPackage pkgName pkgVersion repo commit

constraintSatisfies :: PackageConstraint -> AvailableVersion -> Bool
constraintSatisfies (PackageConstraint constraint) (AvailableVersion{availableVersion = PackageVersion version}) =
  SemVerConstraint.satisfies version constraint

pickVersionHash :: Maybe PackageConstraint -> [AvailableVersion] -> Maybe AvailableVersion
pickVersionHash Nothing (v : _) = Just v
pickVersionHash (Just constraint) versions = find (constraintSatisfies constraint) versions
pickVersionHash _ _ = Nothing

{- | A short, human-readable description of a resolved package:
'<name>@<version> (<short hash>)'.
-}
describe :: Name -> PackageVersion -> GitCommit -> Text
describe name (PackageVersion version) (GitCommit hash) =
  name <> "@" <> toText version <> " (" <> Text.take 8 hash <> ")"

installProject :: TerminalCapabilities -> ExceptT CLIError IO ()
installProject caps = do
  res <- liftIO $ runExceptT loadProjectManifest
  case res of
    Left err ->
      throwError (EPackageError err)
    Right PackageManifest{..} -> do
      let deps = fromMaybe mempty dependencies
      when (not (Map.null deps)) $
        liftIO $
          announce caps "Resolving project dependencies..."
      -- Bind only 'lockEntries': the '..' wildcard would also bind this
      -- record's 'caps' field, shadowing the 'caps' parameter. The 'caps'
      -- below is the parameter (from 'installCommand'), seeding the state.
      InstallState{lockEntries = lockEntries} <-
        flip execStateT (initialInstallState caps) $
          installDependencies deps

      liftIO $ do
        ByteString.writeFile "coal.lock.json" (toStrict (encodePretty (PackageLock lockEntries)))
      liftIO $ announce caps ("Wrote coal.lock.json with " <> showt (Map.size lockEntries) <> " packages")

installCommand :: TerminalCapabilities -> InstallCmdOptions -> ExceptT CLIError IO ()
installCommand caps _opts = installProject caps
