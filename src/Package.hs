{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Package (
  PackageIncludes (..),
  emptyPackageIncludes,
  packageIncludes,
  toModuleNamespace,
) where

import Control.Monad.Except (ExceptT, MonadError (throwError))
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
import Package.Manifest (PackageManifest (..), basePath, filePaths, loadPackageLockManifests, manifestNativeLibraries)
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

{- | Native build inputs collected from a single package, or aggregated across
every package in the lockfile (i.e. all direct and transitive dependencies).

Unlike 'packageCFiles' — whose entries are resolved against the package base
directory — 'packageNativeLibraries' holds bare library names that need no path
resolution and are passed to the linker verbatim as @-l<name>@.
-}
data PackageIncludes = PackageIncludes
  { packageNamespaces :: [(FilePath, Text, [Name])]
  , packageSourceFiles :: [FilePath]
  , packageCFiles :: [FilePath]
  , packageNativeLibraries :: [FilePath]
  }
  deriving (Show, Eq)

emptyPackageIncludes :: PackageIncludes
emptyPackageIncludes = PackageIncludes [] [] [] []

packageIncludes :: ExceptT PackageError IO PackageIncludes
packageIncludes = do
  res <- loadLockFile
  case res of
    Just file ->
      lockIncludes file
    Nothing ->
      throwError ENoLockFile

lockIncludes :: PackageLock -> ExceptT PackageError IO PackageIncludes
lockIncludes lock = do
  manifests <- loadPackageLockManifests lock
  entries <- collectEntries lock manifests
  includes <- traverse entryIncludes entries
  pure
    PackageIncludes
      { packageNamespaces = concatMap packageNamespaces includes
      , packageSourceFiles = concatMap packageSourceFiles includes
      , packageCFiles = concatMap packageCFiles includes
      , packageNativeLibraries = concatMap packageNativeLibraries includes
      }

entryIncludes :: PackageEntry -> ExceptT PackageError IO PackageIncludes
entryIncludes =
  \case
    PackageEntry{packageManifest = manifest@PackageManifest{..}, ..} -> do
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
      pure
        PackageIncludes
          { packageNamespaces = nsEntries
          , packageSourceFiles = absFiles
          , packageCFiles = absCFiles
          , packageNativeLibraries = manifestNativeLibraries manifest
          }
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
