{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Package (packageIncludes, toModuleNamespace) where

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.Char (toUpper)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Extras (Name, forM)
import Package.Entry (PackageEntry (..))
import Package.Error (PackageError (..))
import Package.Lock (LockSpec (..), PackageLock (..), loadLockFile)
import Package.Manifest (PackageManifest (..), basePath, filePaths, loadPackageLockManifests)
import System.Directory (doesFileExist, makeAbsolute)
import System.FilePath ((</>))

{- | Convert a package name to a Coal module namespace prefix.
Capitalizes the first letter of each hyphen-separated segment.
Examples: @"foo"@ → @"Foo"@, @"my-pkg"@ → @"MyPkg"@, @"Coal"@ → @"Coal"@.
-}
toModuleNamespace :: Text -> Text
toModuleNamespace t =
  Text.concat (capitalizeSegment <$> Text.splitOn "-" t)
 where
  capitalizeSegment s = case Text.uncons s of
    Nothing -> ""
    Just (c, rest) -> Text.cons (toUpper c) rest

packageIncludes :: ExceptT PackageError IO ([(FilePath, Text, [Name])], [FilePath], [FilePath])
packageIncludes = do
  res <- loadLockFile
  case res of
    Just file ->
      lockIncludes file
    Nothing ->
      throwError ENoLockFile

lockIncludes :: PackageLock -> ExceptT PackageError IO ([(FilePath, Text, [Name])], [FilePath], [FilePath])
lockIncludes lock = do
  manifests <- loadPackageLockManifests lock
  entries <- collectEntries lock manifests
  includes <- traverse entryIncludes entries
  let (nss, files, cFiles) = unzip3 includes
  pure (concat nss, concat files, concat cFiles)

entryIncludes :: PackageEntry -> ExceptT PackageError IO ([(FilePath, Text, [Name])], [FilePath], [FilePath])
entryIncludes =
  \case
    PackageEntry{packageManifest = PackageManifest{..}, ..} -> do
      relFiles <- filePaths modules (EDependencyInvalidModuleFormat name)
      -- Use absolute paths for package files so that resolveModule cannot
      -- accidentally match them against same-named project source files.
      -- Search all source dirs to find where each file actually lives.
      absFiles <- liftIO $ mapM (resolveInSourceDirs sourcePaths) relFiles
      -- Resolve package C sources relative to the package base path so the
      -- linker can find them inside .coal/packages/<name>/<hash>/.
      absCFiles <- liftIO $ mapM resolveInBase cFiles
      let ns = toModuleNamespace packageName
          nsEntries = [(srcDir, ns, modules) | srcDir <- sourcePaths]
      pure (nsEntries, absFiles, absCFiles)
     where
      sourcePaths = [base </> Text.unpack path | path <- fromMaybe ["src"] source_dirs]
      base = basePath packageName (commit packageSpec)
      cFiles = fromMaybe [] c_sources
      resolveInBase rel = makeAbsolute (base </> rel)

{- | Find a relative file path inside a list of candidate source directories,
returning its absolute path from the first directory that contains it.
Falls back to the first source dir (producing a path that will fail later
with a clear "file not found" message) when no candidate matches.
-}
resolveInSourceDirs :: [FilePath] -> FilePath -> IO FilePath
resolveInSourceDirs [] rel = makeAbsolute rel
resolveInSourceDirs (d : ds) rel = do
  let candidate = d </> rel
  exists <- doesFileExist candidate
  if exists
    then makeAbsolute candidate
    else resolveInSourceDirs ds rel

collectEntries :: PackageLock -> Map Text PackageManifest -> ExceptT PackageError IO [PackageEntry]
collectEntries (PackageLock packages) manifests =
  forM (Map.toList packages) $
    \(pkgName, LockSpec{version, source, commit}) ->
      case Map.lookup pkgName manifests of
        Nothing ->
          throwError (EDependencyManifestMissing pkgName Nothing)
        Just manifest ->
          pure $
            PackageEntry
              { packageName = pkgName
              , packageSpec = LockSpec{version, source, commit}
              , packageManifest = manifest
              }
