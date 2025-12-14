{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Main (main) where

import Coal.Compiler (compile)
import Coal.Compiler.Config (CompilerConfig (..))
import Data.List (nub)
import Options.Applicative
import Version (coalVersion)

data CommandOptions = CommandOptions
  { inputFiles :: [FilePath]
  , outputFile :: FilePath
  , srcPaths :: [FilePath]
  , debugGenerateDotfiles :: Bool
  , debugLLVMOutput :: Bool
  , extraSourceFiles :: [FilePath]
  , silent :: Bool
  }
  deriving (Show)

optionsParser :: Parser CommandOptions
optionsParser = do
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

  pure CommandOptions{..}

main :: IO ()
main = do
  CommandOptions{..} <- execParser optsInfo
  let config =
        CompilerConfig
          { configExecutableName = outputFile
          , configGenerateDotFiles = debugGenerateDotfiles
          , configGenerateLLVMOutput = debugLLVMOutput
          , configSourcePaths = nub ("src" : srcPaths)
          , configCFiles = extraSourceFiles
          , configSilent = silent
          }
  compile config inputFiles

optsInfo :: ParserInfo CommandOptions
optsInfo =
  info
    ( optionsParser
        <**> helper
        <**> infoOption
          coalVersion
          ( long "version"
              <> short 'V'
              <> help "Show compiler version"
          )
    )
    ( fullDesc
        <> progDesc "The Coal compiler command-line interface"
        <> header "Welcome to the Coal compiler"
    )
