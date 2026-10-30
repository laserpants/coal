{-# LANGUAGE OverloadedStrings #-}

import CLI.Command.BuildSpec (buildSpec)
import CLI.Parser.AddCmdSpec (addCmdSpec)
import CLI.Parser.CommandSpec (commandSpec)
import CLI.Parser.UpdateCmdSpec (updateCmdSpec)
import Coal.Compiler.ConfigSpec (configHashSpec)
import Coal.Compiler.Pass.PhaseParsing.ParsingSpec (parsingSpec)
import Coal.Compiler.Pass.PhasePreflight.ExpandLetBindingsSpec (expandLetBindingsSpec)
import Coal.Compiler.PatternMatching.AnomalyDetectionSpec (patternAnomaliesSpec)
import Coal.Kernel.Spec (kernelSpec)
import Coal.Language.Module.ExportSpec (exportMatchingSpec)
import Coal.Language.TypeSpec (typeApplicationSpec, typeArgsSpec)
import Coal.Parser.DoNotationSpec (doNotationSpec)
import Coal.Parser.ExpressionSpec (expressionSpec)
import Coal.Parser.ModuleSpec (moduleHeaderSpec)
import Coal.TypeSystemSpec (typeSystemSpec)
import E2E.Spec (e2eSpec)
import Package.ConstraintValidationSpec (constraintValidationSpec)
import Package.ManifestSpec (manifestSpec)
import Package.ResolutionSpec (resolutionSpec)
import Package.VersionSpec (versionSpec)
import Test.Hspec (SpecWith, describe, hspec)

spec :: SpecWith ()
spec =
  describe "Unit tests" $ do
    configHashSpec
    parsingSpec
    typeSystemSpec
    typeArgsSpec
    typeApplicationSpec
    patternAnomaliesSpec
    expandLetBindingsSpec
    doNotationSpec
    expressionSpec
    moduleHeaderSpec
    exportMatchingSpec

main :: IO ()
main =
  hspec $ do
    spec
    describe "CLI tests" $ do
      addCmdSpec
      commandSpec
      buildSpec
      constraintValidationSpec
      manifestSpec
      resolutionSpec
      versionSpec
      updateCmdSpec

    describe "Kernel tests" kernelSpec
    describe "E2E tests" e2eSpec
