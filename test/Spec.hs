{-# LANGUAGE OverloadedStrings #-}

import CLI.Command.BuildSpec (buildSpec)
import CLI.Parser.AddCmdSpec (addCmdSpec)
import CLI.Parser.CommandSpec (commandSpec)
import CLI.Parser.UpdateCmdSpec (updateCmdSpec)
import Coal.Compiler.Pass.PhasePreflight.ExpandLetBindingsSpec (expandLetBindingsSpec)
import Coal.Compiler.PatternMatching.AnomalyDetectionSpec (patternAnomaliesSpec)
import Coal.Kernel.Spec (kernelSpec)
import Coal.Language.TypeSpec (typeApplicationSpec, typeArgsSpec)
import Coal.TypeSystemSpec (typeSystemSpec)
import E2E.Spec (e2eSpec)
import Package.ConstraintValidationSpec (constraintValidationSpec)
import Package.ResolutionSpec (resolutionSpec)
import Package.VersionSpec (versionSpec)
import Test.Hspec (SpecWith, describe, hspec)

spec :: SpecWith ()
spec =
  describe "Unit tests" $ do
    typeSystemSpec
    typeArgsSpec
    typeApplicationSpec
    patternAnomaliesSpec
    expandLetBindingsSpec

main :: IO ()
main =
  hspec $ do
    spec
    describe "CLI tests" $ do
      addCmdSpec
      commandSpec
      buildSpec
      constraintValidationSpec
      resolutionSpec
      versionSpec
      updateCmdSpec

    describe "Kernel tests" kernelSpec
    describe "E2E tests" e2eSpec
