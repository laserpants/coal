{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Pipeline.Pass.TopLevelFunctionNormalizationSpec (spec) where

import Coal.Kernel.Language.Module (Module (..), moduleObjects)
import Coal.Kernel.Language.Object (FunctionScope (..), Object (..))
import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Pipeline.Invariant (checkTopLevelFunctionsNormalized)
import Coal.Kernel.Pipeline.Pass.TestHelpers (lam1, lam2, lbl, mkDData1, mkModule, runPass, unit_)
import Coal.Kernel.Pipeline.Pass.TopLevelFunctionNormalization (topLevelFunctionNormalization)
import Test.Hspec (Spec, describe, it, shouldBe)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "topLevelFunctionNormalization" $ do
    describe "leaves already-normalized objects unchanged" $ do
      it "function with a non-lambda body" $
        let m = mkModule [DFunction Exported "f" [lbl "x"] unit_]
         in runPass topLevelFunctionNormalization m `shouldBe` Right m

      it "parameterless function with non-lambda body" $
        let m = mkModule [DFunction Exported "f" [] unit_]
         in runPass topLevelFunctionNormalization m `shouldBe` Right m

      it "constant with a literal RHS (not a lambda)" $
        let m = mkModule [DConstant "c" unit_]
         in runPass topLevelFunctionNormalization m `shouldBe` Right m

      it "DExternal is unchanged" $
        let m = mkModule [DExternal "e" TOpq]
         in runPass topLevelFunctionNormalization m `shouldBe` Right m

      it "DData is unchanged" $
        let m = mkModule [mkDData1 "T" "T"]
         in runPass topLevelFunctionNormalization m `shouldBe` Right m

    describe "Rule 1: flattens function bodies that are lambdas" $ do
      it "function foo(x) = fn(y) => e  =>  function foo(x, y) = e" $
        let input = mkModule [DFunction Exported "foo" [lbl "x"] (lam1 "y" unit_)]
            expected = mkModule [DFunction Exported "foo" [lbl "x", lbl "y"] unit_]
         in runPass topLevelFunctionNormalization input `shouldBe` Right expected

      it "parameterless function foo() = fn(x, y) => e  =>  function foo(x, y) = e" $
        let input = mkModule [DFunction Exported "foo" [] (lam2 "x" "y" unit_)]
            expected = mkModule [DFunction Exported "foo" [lbl "x", lbl "y"] unit_]
         in runPass topLevelFunctionNormalization input `shouldBe` Right expected

    describe "Rule 2: promotes constant lambdas to functions" $ do
      it "constant bar = fn(x) => e  =>  function bar(x) = e" $
        let input = mkModule [DConstant "bar" (lam1 "x" unit_)]
            expected = mkModule [DFunction Exported "bar" [lbl "x"] unit_]
         in runPass topLevelFunctionNormalization input `shouldBe` Right expected

      it "constant bar = fn(x, y) => e  =>  function bar(x, y) = e" $
        let input = mkModule [DConstant "bar" (lam2 "x" "y" unit_)]
            expected = mkModule [DFunction Exported "bar" [lbl "x", lbl "y"] unit_]
         in runPass topLevelFunctionNormalization input `shouldBe` Right expected

    describe "invariant: checkTopLevelFunctionsNormalized [] on every object" $ do
      it "after promoting a constant lambda" $
        let input = mkModule [DConstant "bar" (lam1 "x" unit_)]
         in case runPass topLevelFunctionNormalization input of
              Left err -> fail (show err)
              Right m ->
                concatMap checkTopLevelFunctionsNormalized (moduleObjects m)
                  `shouldBe` []

      it "after flattening a function-body lambda" $
        let input = mkModule [DFunction Exported "foo" [lbl "x"] (lam1 "y" unit_)]
         in case runPass topLevelFunctionNormalization input of
              Left err -> fail (show err)
              Right m ->
                concatMap checkTopLevelFunctionsNormalized (moduleObjects m)
                  `shouldBe` []
