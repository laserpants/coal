{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Package.Manifest (
  PackageManifest (..),
  BuildConfig (..),
  basePath,
  loadManifest,
  loadProjectManifest,
  loadPackageLockManifests,
  loadManifestFrom,
  filePaths,
  encodePrettyOrdered,
) where

import CLI.Git.Commit (GitCommit (..))
import Coal.Language.Module.Path (parsePath, toFilePath)
import Control.Monad (unless)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Encode.Pretty (Config (..), defConfig, encodePretty', keyOrder)
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

data BuildConfig = BuildConfig
  { generateDebugArtifacts :: Bool
  , debugLLVMOutput :: Bool
  , silent :: Bool
  , showTiming :: Bool
  , noCache :: Bool
  , sanitize :: Bool
  }
  deriving (Generic, Show, Eq)

{- | All fields default to @False@ so that a partial @"build"@ section in
 @coal.json@ only needs to specify the flags that are enabled.
-}
instance FromJSON BuildConfig where
  parseJSON = withObject "BuildConfig" $ \o ->
    BuildConfig
      <$> o .:? "generate_debug_artifacts" .!= False
      <*> o .:? "debug_llvm_ir" .!= False
      <*> o .:? "silent" .!= False
      <*> o .:? "show_timing" .!= False
      <*> o .:? "no_cache" .!= False
      <*> o .:? "sanitize" .!= False

buildConfigOptions :: Options
buildConfigOptions =
  defaultOptions
    { fieldLabelModifier = go
    }
 where
  go "generateDebugArtifacts" = "generate_debug_artifacts"
  go "debugLLVMOutput" = "debug_llvm_ir"
  go "silent" = "silent"
  go "showTiming" = "show_timing"
  go "noCache" = "no_cache"
  go "sanitize" = "sanitize"
  go other = other

instance ToJSON BuildConfig where
  toJSON = genericToJSON buildConfigOptions

data PackageManifest = PackageManifest
  { name :: Text
  , version :: Maybe PackageVersion
  , modules :: [Text]
  , source_dirs :: Maybe [Text]
  , entry_point :: Maybe Text
  , executable_name :: Maybe FilePath
  , build_config :: Maybe BuildConfig
  , dependencies :: Maybe (Map Text PackageDependency)
  , c_sources :: Maybe [FilePath]
  }
  deriving (Generic, Show, Eq)

instance ToJSON PackageManifest where
  toJSON = genericToJSON defaultOptions{omitNothingFields = True}

instance FromJSON PackageManifest where
  parseJSON = genericParseJSON defaultOptions

{- | Desired field order for coal.json objects. Keys not in this list
 (e.g., dependency names) are placed at the end in their natural order.
-}
manifestFieldOrder :: [Text]
manifestFieldOrder =
  [ "name"
  , "version"
  , "modules"
  , "source_dirs"
  , "entry_point"
  , "executable_name"
  , "build_config"
  , "dependencies"
  , "c_sources"
  , "generate_debug_artifacts"
  , "debug_llvm_ir"
  , "silent"
  , "show_timing"
  , "no_cache"
  , "sanitize"
  , "git"
  ]

{- | Pretty-encode a value to JSON with fields in the documented order.

 Used when writing @coal.json@ so that the output matches the structure
 shown in the documentation.
-}
encodePrettyOrdered :: (ToJSON a) => a -> LazyByteString.ByteString
encodePrettyOrdered = encodePretty' defConfig{confCompare = keyOrder manifestFieldOrder}

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
