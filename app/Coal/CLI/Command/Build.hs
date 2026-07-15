{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.CLI.Command.Build (buildCommand) where

import Coal.CLI.Error (CLIError (..))
import Coal.Compiler (compile)
import Coal.Compiler.Config
import Coal.Package (packageIncludes)
import Coal.Package.Error (PackageError (..))
import Coal.Package.Manifest (PackageManifest (..), filePaths, loadProjectManifest)
import Control.Monad.Except
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Extras (Name)
import Text.Read (readMaybe)

buildCommand :: ExceptT CLIError IO ()
buildCommand = do
  res <- liftIO $ runExceptT $ do
    PackageManifest{..} <- loadProjectManifest

    (srcPaths, inputFiles) <- packageIncludes
    let localSrcPaths = Text.unpack <$> fromMaybe ["src"] source_dirs
        entryPoint = parseEntryPoint entry_point
        config =
          defaultConfig
            { configSourcePaths = nub ("src" : srcPaths <> localSrcPaths)
            , configEntryPoint = entryPoint
            }
    localFiles <- filePaths modules EProjectInvalidModuleFormat
    pure (config, inputFiles <> localFiles)

  case res of
    Left err ->
      throwError (EPackageError err)
    Right (config, files) ->
      liftIO $ compile config files

-- | Parse an entry point string like "Main.main" into (moduleName, functionName)
parseEntryPoint :: Maybe Text -> Maybe (Name, Name)
parseEntryPoint = join . fmap parseDotSeparated
 where
  parseDotSeparated t =
    case Text.splitOn "." t of
      [mod, func] -> Just (mod, func)
      _ -> Nothing
