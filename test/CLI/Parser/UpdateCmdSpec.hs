{-# LANGUAGE OverloadedStrings #-}

module CLI.Parser.UpdateCmdSpec (updateCmdSpec) where

import CLI.Options.UpdateCmd (UpdateCmdOptions (..))
import CLI.Parser.UpdateCmd (updateCmdParser)
import Data.Text (Text)
import Options.Applicative
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

parseUpdate :: [String] -> Either String UpdateCmdOptions
parseUpdate args = case execParserPure defaultPrefs parserInfo args of
  Success r -> Right r
  Failure fr -> Left (show fr)
  CompletionInvoked _ -> Left "completion invoked"
  where
  parserInfo = info updateCmdParser (progDesc "Update dependencies")

updateCmdSpec :: Spec
updateCmdSpec =
  describe "updateCmdParser" $ do
    it "parses no targets as an empty list" $ do
      case parseUpdate [] of
        Right opts -> updateTargets opts `shouldBe` ([] :: [Text])
        Left e -> expectationFailure ("Parse failed: " <> e)

    it "parses a single package name" $ do
      case parseUpdate ["coal-micro-test"] of
        Right opts -> updateTargets opts `shouldBe` ["coal-micro-test"]
        Left e -> expectationFailure ("Parse failed: " <> e)

    it "parses multiple package names" $ do
      case parseUpdate ["coal-pretty", "coal-variant"] of
        Right opts -> updateTargets opts `shouldBe` ["coal-pretty", "coal-variant"]
        Left e -> expectationFailure ("Parse failed: " <> e)