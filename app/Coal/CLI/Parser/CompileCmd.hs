{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.CLI.Parser.CompileCmd (compileCmdParser) where

import Coal.CLI.Options.CompileCmd (CompileCmdOptions (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative

compileCmdParser :: Parser CompileCmdOptions
compileCmdParser = do
  inputFiles <- some (strArgument (metavar "FILES..." <> help "Input files"))

  outputFile <-
    strOption
      ( long "output"
          <> short 'o'
          <> metavar "FILE"
          <> help "Excecutable file name"
      )

  generateDebugArtifacts <-
    switch
      ( long "generate-debug-artifacts"
          <> help "Generate build info and Graphviz DOT files for debugging"
      )

  debugLLVMOutput <-
    switch
      ( long "debug-llvm-ir"
          <> help "Output intermediate LLVM IR"
      )

  srcPaths <-
    many
      ( strOption
          ( long "path"
              <> short 'I'
              <> metavar "FILE"
              <> help "Source file directory path (can be passed multiple times)"
          )
      )

  extraSourceFiles <-
    many
      ( strOption
          ( long "extra-c"
              <> metavar "FILE"
              <> help "Extra C source file (can be passed multiple times)"
          )
      )

  silent <-
    switch
      ( long "silent"
          <> short 's'
          <> help "Supress terminal output"
      )

  noCache <-
    switch
      ( long "no-cache"
          <> help "Disable caching"
      )

  entryPoint <-
    optional
      ( strOption
          ( long "entry-point"
              <> metavar "MODULE.FUNCTION"
              <> help "Entry point module and function (e.g., Main.main)"
          )
      )

  pure CompileCmdOptions{..}
