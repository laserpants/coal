{-# LANGUAGE OverloadedStrings #-}

module CLI.Parser.AddCmdSpec (addCmdSpec) where

import CLI.Options.AddCmd (AddCmdOptions (..))
import CLI.Parser.AddCmd (addCmdParser)
import Options.Applicative
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

parseAdd :: [String] -> Either String AddCmdOptions
parseAdd args = case execParserPure defaultPrefs parserInfo args of
  Success r -> Right r
  Failure fr -> Left (show fr)
  CompletionInvoked _ -> Left "completion invoked"
 where
  parserInfo = info addCmdParser (progDesc "Add dependency")

addCmdSpec :: Spec
addCmdSpec =
  describe "addCmdParser" $ do
    it "parses a Git URL as positional argument" $ do
      case parseAdd ["ssh://git@example.com/user/repo.git"] of
        Right opts -> addUrl opts `shouldBe` "ssh://git@example.com/user/repo.git"
        Left e -> expectationFailure ("Parse failed: " <> e)

    it "parses --version flag" $ do
      case parseAdd ["--version", "^1.0.0", "https://example.com/repo"] of
        Right opts -> do
          addUrl opts `shouldBe` "https://example.com/repo"
          addVersion opts `shouldBe` Just "^1.0.0"
        Left e -> expectationFailure ("Parse failed: " <> e)

    it "parses --name flag" $ do
      case parseAdd ["--name", "my-pkg", "git://example.com/repo"] of
        Right opts -> do
          addUrl opts `shouldBe` "git://example.com/repo"
          addName opts `shouldBe` Just "my-pkg"
        Left e -> expectationFailure ("Parse failed: " <> e)

    it "parses both --version and --name" $ do
      case parseAdd ["--version", "~2.1.0", "--name", "lib", "ssh://host/repo.git"] of
        Right opts -> do
          addUrl opts `shouldBe` "ssh://host/repo.git"
          addVersion opts `shouldBe` Just "~2.1.0"
          addName opts `shouldBe` Just "lib"
        Left e -> expectationFailure ("Parse failed: " <> e)

    it "fails when no URL is provided" $ do
      case parseAdd [] of
        Left _ -> pure ()
        Right _ -> expectationFailure "Should have failed"
