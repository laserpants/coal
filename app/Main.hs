{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Coal.CLI.Command (Command (..))
import Coal.CLI.Command.Build (buildCommand)
import Coal.CLI.Command.Clean (cleanCommand)
import Coal.CLI.Command.Compile (compileCommand)
import Coal.CLI.Command.Install (installCommand)
import Coal.CLI.Command.Version (coalVersion)
import Coal.CLI.Error (prettyCLIError)
import Coal.CLI.Parser.Command (commandParser)
import Control.Monad.Except
import qualified Data.Text.IO as Text
import Options.Applicative

runCommand :: Command -> IO ()
runCommand =
  \case
    CmdCompile opts ->
      compileCommand opts
    CmdBuild -> do
      r <- runExceptT buildCommand
      case r of
        Left err ->
          Text.putStrLn (prettyCLIError err)
        Right{} ->
          pure ()
    CmdClean ->
      cleanCommand
    CmdInstall -> do
      r <- runExceptT installCommand
      case r of
        Left err ->
          Text.putStrLn (prettyCLIError err)
        Right{} ->
          pure ()

versionOption :: Parser (a -> a)
versionOption =
  infoOption
    coalVersion
    ( long "version"
        <> short 'V'
        <> help "Show compiler version"
    )

commandInfo :: ParserInfo Command
commandInfo =
  info
    (commandParser <**> helper <**> versionOption)
    ( fullDesc
        <> progDesc "The Coal compiler command-line interface"
        <> header "Welcome to the Coal compiler"
    )

main :: IO ()
main = do
  cmd <- execParser commandInfo
  runCommand cmd
