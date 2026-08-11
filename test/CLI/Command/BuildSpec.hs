{-# LANGUAGE OverloadedStrings #-}

module CLI.Command.BuildSpec (buildSpec) where

import CLI.Command.Build (deriveExecutableName)
import Data.SemVer (fromText)
import Data.Text (Text)
import Package.Version (PackageVersion (..))
import Test.Hspec

mkVersion :: Text -> PackageVersion
mkVersion t = case fromText t of
  Right v -> PackageVersion v
  Left e  -> error ("Bad test version: " <> e)

buildSpec :: Spec
buildSpec =
  describe "deriveExecutableName" $ do
    it "uses only project name when version is Nothing" $
      deriveExecutableName "my-project" Nothing
        `shouldBe` "my-project"

    it "appends version when present" $
      deriveExecutableName "my-app" (Just (mkVersion "1.2.3"))
        `shouldBe` "my-app-1.2.3"

    it "handles pre-release versions" $
      deriveExecutableName "lib" (Just (mkVersion "0.1.0-alpha.1"))
        `shouldBe` "lib-0.1.0-alpha.1"

    it "handles zero-major version" $
      deriveExecutableName "pkg" (Just (mkVersion "0.0.1"))
        `shouldBe` "pkg-0.0.1"
