{-# LANGUAGE OverloadedStrings #-}

import Coal.Compiler.PatternAnomaliesSpec (patternAnomaliesSpec)
import Coal.TypeSystemSpec (typeSystemSpec)
import E2E.Spec
import Test.Hspec

spec :: SpecWith ()
spec =
  describe "Unit tests" $ do
    typeSystemSpec
    patternAnomaliesSpec

main :: IO ()
main =
  hspec $ do
    spec
    e2eSpec
