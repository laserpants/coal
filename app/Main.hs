{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Main entry point for the Coal compiler CLI.

Detects terminal capabilities at startup and sets UTF-8 encoding explicitly
so the CLI works even when the locale is set to C/POSIX. Unicode output is
degraded gracefully to ASCII on unsupported terminals.
-}
module Main (main) where

import CLI.Command (Command (..))
import CLI.Command.Add (addCommand)
import CLI.Command.Build (buildCommand)
import CLI.Command.Clean (cleanCommand)
import CLI.Command.Compile (compileCommand)
import CLI.Command.Init (initCommand)
import CLI.Command.Install (installCommand)
import CLI.Command.Version (coalVersion)
import CLI.Error (prettyCLIError)
import CLI.Parser.Command (commandParser)
import Coal.Compiler.Terminal (TerminalCapabilities (..), detectStderrCapabilities, sanitizeForTerminal)
import Control.Exception (IOException, try)
import Control.Monad.Except
import qualified Data.Text.IO as Text
import Options.Applicative
import System.IO (hSetEncoding, stderr, stdout, utf8)

runCommand :: TerminalCapabilities -> Command -> IO ()
runCommand caps =
  \case
    CmdAdd opts -> do
      r <- runExceptT (addCommand opts)
      case r of
        Left err ->
          Text.putStrLn (sanitizeForTerminal caps $ "• " <> prettyCLIError err)
        Right{} ->
          pure ()
    CmdCompile opts ->
      compileCommand caps opts
    CmdBuild -> do
      r <- runExceptT (buildCommand caps)
      case r of
        Left err ->
          Text.putStrLn (sanitizeForTerminal caps $ "• " <> prettyCLIError err)
        Right{} ->
          pure ()
    CmdClean ->
      cleanCommand
    CmdInit opts -> do
      r <- runExceptT (initCommand opts)
      case r of
        Left err ->
          Text.putStrLn (sanitizeForTerminal caps $ "• " <> prettyCLIError err)
        Right{} ->
          pure ()
    CmdInstall opts -> do
      r <- runExceptT (installCommand opts)
      case r of
        Left err ->
          Text.putStrLn (sanitizeForTerminal caps $ "• " <> prettyCLIError err)
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
  -- Set UTF-8 encoding explicitly so the CLI works even when the locale is
  -- set to C/POSIX. On modern terminals this is always safe; if encoding
  -- fails, we continue anyway (the terminal capability detection below will
  -- degrade output to ASCII-only).
  _ <- try @IOException (hSetEncoding stdout utf8)
  _ <- try @IOException (hSetEncoding stderr utf8)
  caps <- detectStderrCapabilities
  cmd <- execParser commandInfo
  runCommand caps cmd
