{-# LANGUAGE RecordWildCards #-}

module Coal.CLI.Command.Compile (compileCommand) where

import Coal.CLI.Options.CompileCmd (CompileCmdOptions (..))
import Coal.Compiler (compile)
import Coal.Compiler.Config (CompilerConfig (..))
import Data.List (nub)

compileCommand :: CompileCmdOptions -> IO ()
compileCommand CompileCmdOptions{..} = do
  compile config inputFiles
 where
  config =
    CompilerConfig
      { configExecutableName = outputFile
      , configGenerateDotFiles = debugGenerateDotfiles
      , configGenerateLLVMOutput = debugLLVMOutput
      , configSourcePaths = nub ("src" : srcPaths)
      , configCFiles = extraSourceFiles
      , configSilent = silent
      , configNoCache = noCache
      }
