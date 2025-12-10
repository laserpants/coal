{-# LANGUAGE OverloadedStrings #-}

-- import Coal.Compiler.BuildSpec (buildSpec)
import Coal.Compiler.PatternMatching.AnomalyDetectionSpec (patternAnomaliesSpec)
import Coal.Language.TypeSpec
import Coal.TypeSystemSpec (typeSystemSpec)
import E2E.Kernel.Spec (e2eKernelSpec)
import E2E.Spec
import Test.Hspec

spec :: SpecWith ()
spec =
  describe "Unit tests" $ do
    typeSystemSpec
    listTypeArgsSpec
    typeApplicationSpec
    patternAnomaliesSpec

main :: IO ()
main =
  hspec $ do
    spec
    --    buildSpec
    e2eSpec
    e2eKernelSpec
