{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Package.Manifest (
  PackageManifest (..),
  basePath,
  loadManifest,
  loadProjectManifest,
  loadPackageLockManifests,
  loadManifestFrom,
  filePaths,
) where

import CLI.Git.Commit (GitCommit (..))
import Coal.Language.Module.Path (parsePath, toFilePath)
import Control.Monad (unless)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Either.Extra (eitherToMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Extras (Name, forM)
import GHC.Generics (Generic)
import Package.Dependency (PackageDependency)
import Package.Error (PackageError (..))
import Package.Lock (LockSpec (..), PackageLock (..))
import Package.Version (PackageVersion (..))
import System.Directory
import System.FilePath

data PackageManifest = PackageManifest
  { name :: Text
  , version :: Maybe PackageVersion
  , source_dirs :: Maybe [Text]
  , modules :: [Text]
  , c_sources :: Maybe [FilePath]
  , dependencies :: Maybe (Map Text PackageDependency)
  , entry_point :: Maybe Text
  , executable_name :: Maybe FilePath
  --  , compiler_version :: Text
  }
  deriving (Generic, Show, Eq)

instance ToJSON PackageManifest where
  toJSON = genericToJSON defaultOptions{omitNothingFields = True}

instance FromJSON PackageManifest where
  parseJSON = genericParseJSON defaultOptions

basePath :: Text -> GitCommit -> FilePath
basePath pkgName (GitCommit hash) =
  ".coal"
    </> "packages"
    </> Text.unpack pkgName
    </> Text.unpack hash

loadManifestFrom :: FilePath -> (Text -> PackageError) -> PackageError -> ExceptT PackageError IO PackageManifest
loadManifestFrom filePath failFormat failMissing = do
  exists <- liftIO $ doesFileExist filePath
  unless exists $
    throwError failMissing
  bytes <- liftIO $ LazyByteString.readFile filePath
  case eitherDecode bytes of
    Left err ->
      throwError $ failFormat (Text.pack err)
    Right manifest ->
      pure manifest

loadManifest :: Text -> GitCommit -> ExceptT PackageError IO PackageManifest
loadManifest pkg commit = do
  loadManifestFrom manifestPath (EDependencyInvalidManifestFormat pkg) (EDependencyManifestMissing pkg (Just (Text.pack manifestPath)))
 where
  manifestPath = basePath pkg commit </> "coal.json"

loadProjectManifest :: ExceptT PackageError IO PackageManifest
loadProjectManifest = loadManifestFrom "coal.json" EProjectInvalidManifestFormat EProjectManifestMissing

loadPackageLockManifests :: PackageLock -> ExceptT PackageError IO (Map Text PackageManifest)
loadPackageLockManifests PackageLock{..} = do
  ps <- forM (Map.toList packages) $
    \(name_, spec) -> do
      manifest <- loadManifest name_ (commit spec)
      pure (name_, manifest)
  pure (Map.fromList ps)

filePaths :: [Name] -> (Name -> PackageError) -> ExceptT PackageError IO [FilePath]
filePaths names fail_ = ExceptT $ pure $ traverse go names
 where
  go name =
    maybe
      (throwError (fail_ name))
      (Right . toFilePath)
      (eitherToMaybe $ parsePath name)
