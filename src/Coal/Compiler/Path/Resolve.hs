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
      "Component must start with an upper-case letter or underscore: " ++ show c
    InvalidChar c ->
      "Component contains invalid characters: " ++ show c

isDirectoryPrefix :: FilePath -> FilePath -> Bool
isDirectoryPrefix root p =
  length rootComps <= length pathComps
    && rootComps == take (length rootComps) pathComps
 where
  rootComps = filter (not . null) $ splitDirectories (normalise root)
  pathComps = filter (not . null) $ splitDirectories (normalise p)

resolveModule :: [FilePath] -> FilePath -> IO (Either String (FilePath, FilePath, Text))
resolveModule roots fp =
  runExceptT $ do
    roots' <- liftIO $ mapM canonicalizePath roots

    let candidates =
          if isAbsolute fp
            then [fp]
            else [r </> fp | r <- roots']

    -- Find first existing candidate
    existing <- liftIO $ filterM doesFileExist candidates
    chosen <- case existing of
      [] ->
        throwError $ "File not found. Tried these candidate paths:\n  " ++ unlines candidates
      (c : _) ->
        return c

    -- Canonicalize the chosen file
    canFile <- liftIO $ canonicalizePath chosen

    -- Find all roots that are directory prefixes of the canonical file
    let matching = filter (`isDirectoryPrefix` canFile) roots'
    bestRoot <-
      case matching of
        [] ->
          throwError $ "File is not inside any source root. Canonical file: " ++ canFile
        _ ->
          return $ maximumBy (comparing length) matching

    -- Compute relative path and check extension
    let rel = makeRelative bestRoot canFile
    when (takeExtension rel /= ".coal") $
      throwError $
        "Unsupported extension: " ++ takeExtension rel ++ " (file: " ++ canFile ++ ")"

    -- Split into components and validate
    let relNoExt = dropExtension rel
        comps = splitDirectories relNoExt
    case traverse (validateComponent . Text.pack) comps of
      Left err ->
        throwError (errorMessage err)
      Right{} ->
        return ()

    return (canFile, bestRoot, Text.pack (intercalate "." comps))
