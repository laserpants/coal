{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Pipeline.Pass.LambdaFlatteningSpec (spec) where

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..))
import Coal.Kernel.Language.Module (Module (..), moduleObjects)
import Coal.Kernel.Language.Object (FunctionScope (..), Object (..))
import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Pipeline.Invariant (checkLambdasFlattened)
import Coal.Kernel.Pipeline.Pass.LambdaFlattening (lambdaFlattening)
import Coal.Kernel.Pipeline.Pass.TestHelpers (lam1, lam2, lbl, mkModule, ne, runPass, unit_)
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

lam3 :: Text -> Text -> Text -> Expr Type -> Expr Type
lam3 p q r = ELam (ne [lbl p, lbl q, lbl r])

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "lambdaFlattening" $ do
    describe "leaves already-flat expressions unchanged" $ do
      it "literal (no lambdas)" $
        let m = mkModule [DFunction Exported "f" [] unit_]
         in runPass lambdaFlattening m `shouldBe` Right m

      it "single-param lambda" $
        let m = mkModule [DFunction Exported "f" [] (lam1 "x" unit_)]
         in runPass lambdaFlattening m `shouldBe` Right m

      it "two-param lambda (already flat)" $
        let m = mkModule [DFunction Exported "f" [] (lam2 "x" "y" unit_)]
         in runPass lambdaFlattening m `shouldBe` Right m

      it "lambda whose body is a let (not another lambda)" $
        let m = mkModule [DFunction Exported "f" [] (lam1 "x" (ELet (ne [Binding (lbl "r") unit_]) unit_))]
         in runPass lambdaFlattening m `shouldBe` Right m

    describe "flattens nested lambda chains" $ do
      it "fn(x) => fn(y) => e  =>  fn(x,y) => e" $
        let input = mkModule [DFunction Exported "f" [] (lam1 "x" (lam1 "y" unit_))]
            expected = mkModule [DFunction Exported "f" [] (lam2 "x" "y" unit_)]
         in runPass lambdaFlattening input `shouldBe` Right expected

      it "fn(x) => fn(y) => fn(z) => e  =>  fn(x,y,z) => e" $
        let input = mkModule [DFunction Exported "f" [] (lam1 "x" (lam1 "y" (lam1 "z" unit_)))]
            expected = mkModule [DFunction Exported "f" [] (lam3 "x" "y" "z" unit_)]
         in runPass lambdaFlattening input `shouldBe` Right expected

      it "fn(x,y) => fn(z) => e  =>  fn(x,y,z) => e  (multi-param outer)" $
        let input = mkModule [DFunction Exported "f" [] (lam2 "x" "y" (lam1 "z" unit_))]
            expected = mkModule [DFunction Exported "f" [] (lam3 "x" "y" "z" unit_)]
         in runPass lambdaFlattening input `shouldBe` Right expected

      it "lambda inside a let RHS also flattened" $
        let nested = lam1 "a" (lam1 "b" unit_)
            flat = lam2 "a" "b" unit_
            input = mkModule [DFunction Exported "f" [] (ELet (ne [Binding (lbl "r") nested]) unit_)]
            expected = mkModule [DFunction Exported "f" [] (ELet (ne [Binding (lbl "r") flat]) unit_)]
         in runPass lambdaFlattening input `shouldBe` Right expected

      it "lambda inside a case clause body also flattened" $
        let nested = lam1 "a" (lam1 "b" unit_)
            flat = lam2 "a" "b" unit_
            input = mkModule [DFunction Exported "f" [] (ECase TOpq unit_ (ne [Clause (ne [lbl "C"]) nested]))]
            expected = mkModule [DFunction Exported "f" [] (ECase TOpq unit_ (ne [Clause (ne [lbl "C"]) flat]))]
         in runPass lambdaFlattening input `shouldBe` Right expected

    describe "invariant: checkLambdasFlattened [] on every object body" $ do
      it "after flattening a two-deep nested lambda" $
        let input = mkModule [DFunction Exported "f" [] (lam1 "x" (lam1 "y" unit_))]
         in case runPass lambdaFlattening input of
              Left err -> fail (show err)
              Right m ->
                concatMap
                  ( \case
                      DFunction _ _ _ body -> checkLambdasFlattened body
                      DConstant _ body -> checkLambdasFlattened body
                      _ -> []
                  )
                  (moduleObjects m)
                  `shouldBe` []

      it "after flattening a three-deep nested lambda" $
        let input = mkModule [DFunction Exported "f" [] (lam1 "x" (lam1 "y" (lam1 "z" unit_)))]
         in case runPass lambdaFlattening input of
              Left err -> fail (show err)
              Right m ->
                concatMap
                  ( \case
                      DFunction _ _ _ body -> checkLambdasFlattened body
                      DConstant _ body -> checkLambdasFlattened body
                      _ -> []
                  )
                  (moduleObjects m)
                  `shouldBe` []

    describe "idempotence" $ do
      it "running the pass twice is the same as running it once" $
        let input = mkModule [DFunction Exported "f" [] (lam1 "x" (lam1 "y" unit_))]
         in case runPass lambdaFlattening input of
              Left e -> fail (show e)
              Right m1 ->
                runPass lambdaFlattening m1 `shouldBe` Right m1
