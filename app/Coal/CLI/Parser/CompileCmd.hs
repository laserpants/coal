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

  debugGenerateDotfiles <-
    switch
      ( long "debug-generate-dotfiles"
          <> help "Generate Graphviz DOT files for debugging"
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

  pure CompileCmdOptions{..}
