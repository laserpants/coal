{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.CLI.Parser.CompileCmd (compileCmdParser) where

import Coal.CLI.Options.CompileCmd (CompileCmdOptions (..))
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

  pure CompileCmdOptions{..}
