{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the @coal.json@ manifest JSON encoding, focused on the
@native_libraries@ field that declares native libraries to link against.

The field is optional and must round-trip: absent manifests parse to
@Nothing@, present manifests preserve their values, and @Nothing@ is omitted
from encoded output so that existing manifests are unchanged.
-}
module Package.ManifestSpec (manifestSpec) where

import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Lazy as LazyByteString
import Package.Manifest (PackageManifest (..), encodePrettyOrdered)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

decodeManifest :: LazyByteString.ByteString -> Either String PackageManifest
decodeManifest = eitherDecode

minimalManifest :: PackageManifest
minimalManifest =
  PackageManifest
    { name = "demo"
    , version = Nothing
    , modules = ["Main"]
    , source_dirs = Just ["src"]
    , entry_point = Just "Main.main"
    , executable_name = Nothing
    , build_config = Nothing
    , dependencies = Nothing
    , c_sources = Nothing
    , native_libraries = Nothing
    }

manifestSpec :: Spec
manifestSpec =
  describe "PackageManifest JSON" $ do
    it "parses native_libraries" $
      case decodeManifest "{\"name\":\"demo\",\"modules\":[\"Main\"],\"native_libraries\":[\"sqlite3\",\"z\"]}" of
        Right m -> native_libraries m `shouldBe` Just ["sqlite3", "z"]
        Left e -> expectationFailure ("Parse failed: " <> e)

    it "defaults native_libraries to Nothing when absent" $
      case decodeManifest "{\"name\":\"demo\",\"modules\":[\"Main\"]}" of
        Right m -> native_libraries m `shouldBe` Nothing
        Left e -> expectationFailure ("Parse failed: " <> e)

    it "omits native_libraries from encoded output when Nothing" $
      LazyByteString.toStrict (encode minimalManifest)
        `shouldSatisfy` (not . ByteString.Char8.isInfixOf "native_libraries")

    it "includes native_libraries in pretty output when present" $
      LazyByteString.toStrict (encodePrettyOrdered minimalManifest{native_libraries = Just ["sqlite3"]})
        `shouldSatisfy` ByteString.Char8.isInfixOf "native_libraries"
