{-# LANGUAGE OverloadedStrings #-}

import CLI.Command.BuildSpec (buildSpec)
import CLI.Parser.InstallCmdSpec (installCmdSpec)
import Coal.Compiler.PatternMatching.AnomalyDetectionSpec (patternAnomaliesSpec)
import Coal.Kernel.Spec (kernelSpec)
import Coal.Language.TypeSpec (typeApplicationSpec, typeArgsSpec)
import Coal.TypeSystemSpec (typeSystemSpec)
import CLI.Parser.AddCmdSpec (addCmdSpec)
import E2E.Spec (e2eSpec)
import Package.VersionSpec (versionSpec)
import Test.Hspec (SpecWith, describe, hspec)

spec :: SpecWith ()
spec =
  describe "Unit tests" $ do
    typeSystemSpec
    typeArgsSpec
    typeApplicationSpec
    patternAnomaliesSpec

main :: IO ()
main =
  hspec $ do
    spec
    describe "CLI tests" $ do
      addCmdSpec
      buildSpec
      versionSpec
      installCmdSpec

    describe "Kernel tests" kernelSpec
    describe "E2E tests" e2eSpec
