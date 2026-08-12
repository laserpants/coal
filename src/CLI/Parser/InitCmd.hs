{-# LANGUAGE OverloadedStrings #-}

module CLI.Parser.InitCmd (initCmdParser) where

import CLI.Options.InitCmd (InitCmdOptions (..))
import qualified Data.Text as Text
import Options.Applicative

initCmdParser :: Parser InitCmdOptions
initCmdParser =
  InitCmdOptions
    <$> optional
      ( option
          (Text.pack <$> str)
          ( long "name"
              <> metavar "NAME"
              <> help "Project name (defaults to directory name"
          )
      )
    <*> switch
      ( long "force"
          <> help "Overwrite existing project files"
      )
