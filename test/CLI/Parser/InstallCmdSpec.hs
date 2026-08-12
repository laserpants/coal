{-# LANGUAGE OverloadedStrings #-}

module CLI.Parser.InstallCmdSpec (installCmdSpec) where

import CLI.Options.InstallCmd (InstallCmdOptions (..))
import CLI.Parser.InstallCmd (installCmdParser)
import Options.Applicative
import Test.Hspec

parseInstall :: [String] -> Either String InstallCmdOptions
parseInstall args = case execParserPure defaultPrefs parserInfo args of
  Success r -> Right r
  Failure fr -> Left (show fr)
  CompletionInvoked _ -> Left "completion invoked"
 where
  parserInfo = info installCmdParser (progDesc "Install packages")

installCmdSpec :: Spec
installCmdSpec =
  describe "installCmdParser" $ do
    it "defaults target to '.'" $ do
      case parseInstall [] of
        Right opts -> installTarget opts `shouldBe` "."
        Left e -> expectationFailure ("Parse failed: " <> e)

    it "parses --target flag" $ do
      case parseInstall ["--target", "/some/path"] of
        Right opts -> installTarget opts `shouldBe` "/some/path"
        Left e -> expectationFailure ("Parse failed: " <> e)

    it "parses --revision flag" $ do
      case parseInstall ["--revision", "abc123"] of
        Right opts -> installRevision opts `shouldBe` Just "abc123"
        Left e -> expectationFailure ("Parse failed: " <> e)

    it "parses both --target and --revision" $ do
      case parseInstall ["--target", "dir", "--revision", "def456"] of
        Right opts -> do
          installTarget opts `shouldBe` "dir"
          installRevision opts `shouldBe` Just "def456"
        Left e -> expectationFailure ("Parse failed: " <> e)

    it "revision is Nothing when not provided" $ do
      case parseInstall [] of
        Right opts -> installRevision opts `shouldBe` Nothing
        Left e -> expectationFailure ("Parse failed: " <> e)
