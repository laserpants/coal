{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Package (packageIncludes) where

import Coal.Package.Entry (PackageEntry (..))
import Coal.Package.Error (PackageError (..))
import Coal.Package.Lock (LockSpec (..), PackageLock (..), loadLockFile)
import Coal.Package.Manifest (PackageManifest (..), basePath, filePaths, loadPackageLockManifests)
import Control.Monad.Except
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tuple.Extra (both)
import System.FilePath

packageIncludes :: ExceptT PackageError IO ([FilePath], [FilePath])
packageIncludes = do
  res <- loadLockFile
  case res of
    Just file ->
      lockIncludes file
    Nothing ->
      throwError ENoLockFile

lockIncludes :: PackageLock -> ExceptT PackageError IO ([FilePath], [FilePath])
lockIncludes lock = do
  manifests <- loadPackageLockManifests lock
  entries <- collectEntries lock manifests
  includes <- traverse entryIncludes entries
  pure (both concat (unzip includes))

entryIncludes :: PackageEntry -> ExceptT PackageError IO ([FilePath], [FilePath])
entryIncludes =
  \case
    PackageEntry{packageManifest = PackageManifest{..}, ..} -> do
      files <- filePaths modules (EDependencyInvalidModuleFormat name)
      pure (sourcePaths, files)
     where
      sourcePaths = [base </> Text.unpack path | path <- fromMaybe ["src"] source_dirs]
      base = basePath packageName (commit packageSpec)

collectEntries :: PackageLock -> Map Text PackageManifest -> ExceptT PackageError IO [PackageEntry]
collectEntries (PackageLock packages) manifests =
  forM (Map.toList packages) $
    \(pkgName, LockSpec{..}) ->
      case Map.lookup pkgName manifests of
        Nothing ->
          throwError (EDependencyManifestMissing pkgName Nothing)
        Just manifest ->
          pure $
            PackageEntry
              { packageName = pkgName
              , packageSpec = LockSpec{..}
              , packageManifest = manifest
              }
