{-# LANGUAGE OverloadedStrings #-}

import Coal.Compiler.PatternMatching.AnomalyDetectionSpec (patternAnomaliesSpec)
import Coal.Kernel.Spec (kernelSpec)
import Coal.Language.TypeSpec (typeApplicationSpec, typeArgsSpec)
import Coal.TypeSystemSpec (typeSystemSpec)
import CLI.Parser.AddCmdSpec (addCmdSpec)
import E2E.Spec (e2eSpec)
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
    describe "Kernel tests" kernelSpec
    --    buildSpec
    describe "E2E tests" e2eSpec

--    e2eKernelSpec
