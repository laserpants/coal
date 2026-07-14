{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.CLI.Command.Compile (compileCommand) where

import Coal.CLI.Options.CompileCmd (CompileCmdOptions (..))
import Coal.Compiler (compile)
import Coal.Compiler.Config (CompilerConfig (..))
import Control.Monad (join)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as Text

compileCommand :: CompileCmdOptions -> IO ()
compileCommand CompileCmdOptions{..} = do
  compile config inputFiles
 where
  parsedEntryPoint = parseEntryPoint entryPoint
  config =
    CompilerConfig
      { configExecutableName = outputFile
      , configGenerateDebugArtifacts = generateDebugArtifacts
      , configGenerateLLVMOutput = debugLLVMOutput
      , configSourcePaths = nub ("src" : srcPaths)
      , configCFiles = extraSourceFiles
      , configSilent = silent
      , configNoCache = noCache
      , configEntryPoint = parsedEntryPoint
      }

-- | Parse an entry point string like "Main.main" into (moduleName, functionName)
parseEntryPoint :: Maybe Text -> Maybe (Text, Text)
parseEntryPoint = join . fmap parseDotSeparated
 where
  parseDotSeparated t =
    case Text.breakOnEnd "." t of
      (func, "") -> Nothing
      (func, ".") -> Nothing
      (_, func) ->
        case Text.unsnoc (Text.dropAround (== '.') t) of
          Nothing -> Nothing
          Just (mod, '.') ->
            Just (mod, func)
          _ -> Nothing
