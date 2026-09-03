{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CLI.Command.Add (addCommand) where

import CLI.Command.Install (installProject)
import CLI.Error (CLIError (..))
import CLI.Git (gitCloneRepo)
import CLI.Git.Repo (GitRepo (..))
import CLI.Options.AddCmd (AddCmdOptions (..))
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (toStrict)
import qualified Data.ByteString as ByteString
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.SemVer.Constraint (Constraint (CAny))
import qualified Data.SemVer.Constraint as SemVerConstraint
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Package.Dependency (PackageDependency (..))
import Package.Error (PackageError (..))
import Package.Manifest (PackageManifest (..), encodePrettyOrdered, loadManifestFrom, loadProjectManifest)
import Package.Version (PackageConstraint (..))
import System.IO.Temp (withSystemTempDirectory)

addCommand :: AddCmdOptions -> ExceptT CLIError IO ()
addCommand AddCmdOptions{..} = do
  manifest <- withExceptT EPackageError loadProjectManifest
  let repo = GitRepo (Text.pack addUrl)
  pkgName <- case addName of
    Just name -> pure name
    Nothing -> deriveNameFromRepo repo
  let constraint =
        PackageConstraint <$> case addVersion of
          Nothing -> Just CAny
          Just txt ->
            case SemVerConstraint.fromText txt of
              Right c -> Just c
              Left _ -> Just CAny
      dep = PackageDependency{version = constraint, git = repo}
  let deps = fromMaybe mempty (dependencies manifest)
      newDeps = Map.insert pkgName dep deps
      newManifest = manifest{dependencies = Just newDeps}
  liftIO $ ByteString.writeFile "coal.json" (toStrict (encodePrettyOrdered newManifest))
  installProject
  liftIO $ Text.putStrLn ("Added dependency: " <> pkgName)

deriveNameFromRepo :: GitRepo -> ExceptT CLIError IO Text
deriveNameFromRepo repo = do
  withSystemTempDirectory "coal-add" $ \tmpDir -> do
    gitCloneRepo repo tmpDir
    let manifestPath = tmpDir <> "/coal.json"
    result <-
      liftIO $
        runExceptT $
          loadManifestFrom
            manifestPath
            (\err -> EProjectInvalidManifestFormat ("invalid JSON: " <> err))
            ( EProjectInvalidManifestFormat
                ("cloned repository at " <> repoUrl repo <> " has no coal.json")
            )
    case result of
      Left err -> throwError (EPackageError err)
      Right PackageManifest{name} -> pure name
