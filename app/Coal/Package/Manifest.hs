{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Package.Manifest (
  PackageManifest (..),
  basePath,
  loadManifest,
  loadProjectManifest,
  loadPackageLockManifests,
  loadManifestFrom,
  filePaths,
) where

import Coal.CLI.Git.Commit (GitCommit (..))
import Coal.Language.Module.Path (parsePath, toFilePath)
import Coal.Package.Dependency (PackageDependency)
import Coal.Package.Error (PackageError (..))
import Coal.Package.Lock (LockSpec (..), PackageLock (..))
import Coal.Package.Version (PackageVersion (..))
import Control.Monad.Except
import Data.Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Extras (Name)
import GHC.Generics (Generic)
import System.Directory
import System.FilePath
import Data.Either.Extra (eitherToMaybe)

data PackageManifest = PackageManifest
  { name :: Text
  , version :: Maybe PackageVersion
  , source_dirs :: Maybe [Text]
  , modules :: [Text]
  , dependencies :: Maybe (Map Text PackageDependency)
  --  , compiler_version :: Text
  }
  deriving (Generic, Show, Eq)

instance ToJSON PackageManifest where
  toJSON = genericToJSON defaultOptions

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
