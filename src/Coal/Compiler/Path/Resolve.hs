{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Path.Resolve (resolveModule) where

import Coal.Language.Module.Path (PathError (..), validateComponent)
import Control.Monad (filterM, when)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate, maximumBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath (dropExtension, isAbsolute, makeRelative, normalise, splitDirectories, takeExtension, (</>))

errorMessage :: PathError -> String
errorMessage =
  \case
    EmptyComponent ->
      "Empty path component"
    InvalidStart c ->
      "Component must start with an upper-case letter or underscore: " <> show c
    InvalidChar c ->
      "Component contains invalid characters: " <> show c

isDirectoryPrefix :: FilePath -> FilePath -> Bool
isDirectoryPrefix root path =
  length rootComps <= length pathComps
    && rootComps == take (length rootComps) pathComps
 where
  rootComps = comps root
  pathComps = comps path
  comps p = filter (not . null) $ splitDirectories (normalise p)

{- | Resolve a module file against a set of source roots.

The first argument lists the roots that are searched for a /relative/ path —
these are the project's own source directories, and deliberately exclude
installed-package source roots so that a locally-declared module can never
silently resolve to a same-named file inside a dependency. The second argument
lists every known root (project and package); it is only used to classify the
resolved file, i.e. to pick the longest matching root and derive the module name
from the path relative to it. Absolute paths are never searched, but are still
classified against the known roots.

Returns the canonical file path, the best-matching root, and the derived
(dot-separated) module name.
-}
resolveModule :: [FilePath] -> [FilePath] -> FilePath -> IO (Either String (FilePath, FilePath, Text))
resolveModule searchRoots knownRoots fp =
  runExceptT $ do
    searchRoots' <- liftIO $ mapM canonicalizePath searchRoots
    knownRoots' <- liftIO $ mapM canonicalizePath knownRoots

    let candidates =
          if isAbsolute fp
            then [fp]
            else [r </> fp | r <- searchRoots']

    -- Find first existing candidate
    existing <- liftIO $ filterM doesFileExist candidates
    choice <-
      case existing of
        [] ->
          throwError $ "File not found. Tried these candidate paths:\n  " <> unlines candidates
        (c : _) ->
          return c

    -- Canonicalize the chosen file
    canFile <- liftIO $ canonicalizePath choice

    -- Find all known roots that are directory prefixes of the canonical file
    let matching = filter (`isDirectoryPrefix` canFile) knownRoots'
    bestRoot <-
      case matching of
        [] ->
          throwError $ "File is not inside any source root. Canonical file: " <> canFile
        _ ->
          return $ maximumBy (comparing length) matching

    -- Compute relative path and check extension
    let rel = makeRelative bestRoot canFile
    when (takeExtension rel /= ".coal") $
      throwError $
        "Unsupported extension: " <> takeExtension rel <> " (file: " <> canFile <> ")"

    -- Split into components and validate
    let relNoExt = dropExtension rel
        comps = splitDirectories relNoExt
    case traverse (validateComponent . Text.pack) comps of
      Left err ->
        throwError (errorMessage err)
      Right{} ->
        return ()

    return (canFile, bestRoot, Text.pack (intercalate "." comps))
