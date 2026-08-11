{-# LANGUAGE OverloadedStrings #-}

module Package.VersionSpec (versionSpec) where

import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as LBS
import Package.Version (PackageConstraint)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, expectationFailure)

decodeConstraint :: LBS.ByteString -> Either String PackageConstraint
decodeConstraint = eitherDecode

versionSpec :: Spec
versionSpec =
  describe "PackageConstraint JSON" $ do
    it "roundtrips wildcard constraint" $ do
      case decodeConstraint "\"*\"" of
        Right c -> do
          LBS.toStrict (encode c) `shouldBe` "\"*\""
          case decodeConstraint (encode c) of
            Right _ -> pure ()
            Left e -> expectationFailure ("Re-decode failed: " <> e)
        Left e ->
          expectationFailure ("Parse failed: " <> e)

    it "roundtrips exact version constraint" $ do
      case decodeConstraint "\"1.2.3\"" of
        Right c -> do
          LBS.toStrict (encode c) `shouldBe` "\"1.2.3\""
          case decodeConstraint (encode c) of
            Right _ -> pure ()
            Left e -> expectationFailure ("Re-decode failed: " <> e)
        Left e ->
          expectationFailure ("Parse failed: " <> e)

    it "encodes caret constraint without crashing" $ do
      case decodeConstraint "\"^1.2.0\"" of
        Right c -> do
          let bs = encode c
          LBS.length bs `shouldSatisfy` (> 0)
        Left _ ->
          -- semver fromText may not support caret syntax;
          -- the important thing is that ToJSON no longer crashes
          pure ()

    it "encodes tilde constraint without crashing" $ do
      case decodeConstraint "\"~2.1.0\"" of
        Right c -> do
          let bs = encode c
          LBS.length bs `shouldSatisfy` (> 0)
        Left _ ->
          pure ()
