{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CLI.Command.Compile (compileCommand) where

import CLI.Command.EntryPoint (parseEntryPoint)
import CLI.Options.CompileCmd (CompileCmdOptions (..))
import Coal.Compiler (compile)
import Coal.Compiler.Config (CompilerConfig (..))
import Coal.Compiler.Terminal (TerminalCapabilities)
import Data.List (nub)
import System.Exit (ExitCode (ExitFailure), exitWith)

compileCommand :: TerminalCapabilities -> CompileCmdOptions -> IO ()
compileCommand caps CompileCmdOptions{..} = do
  result <- compile caps config inputFiles
  case result of
    Left{} ->
      exitWith (ExitFailure 1)
    Right{} ->
      pure ()
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
      , configShowTiming = showTiming
      , configNoCache = noCache
      , configEntryPoint = parsedEntryPoint
      , configPackageNamespaces = []
      , configSanitize = sanitize
      }
