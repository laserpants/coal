{-# LANGUAGE OverloadedStrings #-}

module CLI.Parser.CommandSpec (commandSpec) where

import CLI.Command (Command (..))
import CLI.Options.Command (CommandOptions (..))
import CLI.Options.UpdateCmd (UpdateCmdOptions (..))
import CLI.Parser.Command (commandOptionsParser)
import Options.Applicative hiding (command)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

parseCommand :: [String] -> Either String CommandOptions
parseCommand args = case execParserPure defaultPrefs parserInfo args of
  Success r -> Right r
  Failure fr -> Left (show fr)
  CompletionInvoked _ -> Left "completion invoked"
 where
  parserInfo = info commandOptionsParser (progDesc "Coal CLI")

commandSpec :: Spec
commandSpec =
  describe "commandOptionsParser" $ do
    it "fails when no subcommand is given" $
      case parseCommand [] of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected the parse to fail without a subcommand"

    it "parses install with no directory" $
      case parseCommand ["install"] of
        Right opts -> do
          commandDir opts `shouldBe` (Nothing :: Maybe String)
          case command opts of
            CmdInstall -> pure ()
            _ -> expectationFailure "expected CmdInstall"
        Left e -> expectationFailure ("Parse failed: " <> e)

    it "parses a global -C/--directory before install" $
      case parseCommand ["-C", "/tmp/proj", "install"] of
        Right opts -> commandDir opts `shouldBe` Just "/tmp/proj"
        Left e -> expectationFailure ("Parse failed: " <> e)

    it "parses --directory before update targets" $
      case parseCommand ["--directory", "/tmp/proj", "update", "coal-micro-test"] of
        Right opts -> do
          commandDir opts `shouldBe` Just "/tmp/proj"
          case command opts of
            CmdUpdate UpdateCmdOptions{updateTargets = [t]} ->
              t `shouldBe` "coal-micro-test"
            _ -> expectationFailure "expected CmdUpdate"
        Left e -> expectationFailure ("Parse failed: " <> e)
