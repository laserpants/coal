{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Coal.Compiler (compile)
import Coal.Compiler.Config (CompilerConfig (..))
import Options.Applicative

data CommandOptions = CommandOptions
  { inputFiles :: [FilePath]
  , outputFile :: FilePath
  , debugGenerateDotfiles :: Bool
  , debugLLVMOutput :: Bool
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

  pure CommandOptions{..}

main :: IO ()
main = do
  CommandOptions{..} <- execParser optsInfo
  let config =
        CompilerConfig
          { configExecutableName = outputFile
          , configGenerateDotFiles = debugGenerateDotfiles
          , configGenerateLLVMOutput = debugLLVMOutput
          }
  compile config inputFiles

optsInfo :: ParserInfo CommandOptions
optsInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "The Coal compiler command-line interface"
        <> header "Welcome to the Coal compiler"
    )
