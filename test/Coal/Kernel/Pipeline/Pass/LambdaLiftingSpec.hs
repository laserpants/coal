{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Pipeline.Pass.LambdaLiftingSpec (spec) where

import Coal.Kernel.Language.Expr (Binding (..), Expr (..), Label (..))
import Coal.Kernel.Language.Module (Module (..), moduleObjects)
import Coal.Kernel.Language.Object (FunctionScope (..), Object (..))
import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Language.Type.HasType (foldType, typeOf)
import Coal.Kernel.Pipeline.Invariant (checkLambdasLifted)
import Coal.Kernel.Pipeline.Pass.LambdaLifting (lambdaLifting)
import Coal.Kernel.Pipeline.Pass.TestHelpers (lbl, mkModule, ne, runPass, unit_, var)
import Test.Hspec (Spec, describe, it, shouldBe)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | The type of a lambda @fn(p1..pn) => body@ given parameter labels and body.
lamType :: [Label Type] -> Expr Type -> Type
lamType params body = foldType (typeOf body) (map typeOf params)

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "lambdaLifting" $ do
    describe "lifts lambdas with no free variables" $ do
      it "fn(x) => unit  =>  DFunction lam.0 [x] unit; call site EVar lam.0" $
        let params = ne [lbl "x"]
            body = unit_
            lambdaTy = lamType [lbl "x"] body
            liftedName = "lam.0"
            input = mkModule [DConstant "c" (ELam params body)]
            expectedCallSite = EVar (Label lambdaTy liftedName)
            expectedLiftedFn = DFunction Local liftedName [lbl "x"] body
            expectedModule = mkModule [DConstant "c" expectedCallSite, expectedLiftedFn]
         in runPass lambdaLifting input `shouldBe` Right expectedModule

    describe "lifts lambdas with free variables" $ do
      it "fn(x) => y (y free)  =>  DFunction lam.0 [y, x] y; call site EApp lam.0 [y]" $
        -- y is a free variable; sorted before x alphabetically... wait "x" < "y"
        -- so sorted order is [x, y]? No: "x" < "y" so fv params in sorted order is [y]
        -- fvLabels = [Label TOpq "y"] (sorted by name, one free var)
        -- allParams = [lbl "y", lbl "x"] (fv first, then lambda params)
        -- liftedFnType = TOpq ~> TOpq ~> TOpq (y:TOpq -> x:TOpq -> body:TOpq)
        -- Actually body = EVar (lbl "x") ... let me reconsider.
        -- body = var "y" (just references y)
        -- typeOf body = TOpq (from lbl)
        -- fvLabels = [Label TOpq "y"]  (y is free, x is a lambda param)
        -- allParams = [Label TOpq "y", Label TOpq "x"]
        -- liftedFnType = foldType TOpq [TOpq, TOpq] = TOpq ~> TOpq ~> TOpq
        -- lambdaType = foldType TOpq [TOpq] = TOpq ~> TOpq (original fn(x)=>y type)
        -- call site = EApp lambdaType (EVar (Label liftedFnType "lam.0")) [EVar (lbl "y")]
        let bodyExpr = var "y"
            lamParam = lbl "x"
            fvLabel = lbl "y"
            allParamLabels = [fvLabel, lamParam]
            liftedFnType = foldType TOpq (map typeOf allParamLabels)
            lambdaType = foldType TOpq [typeOf lamParam]
            liftedName = "lam.0"
            input = mkModule [DConstant "c" (ELam (ne [lamParam]) bodyExpr)]
            expectedLiftedFn = DFunction Local liftedName [fvLabel, lamParam] bodyExpr
            expectedCallSite = EApp lambdaType (EVar (Label liftedFnType liftedName)) (ne [EVar fvLabel])
            expectedModule = mkModule [DConstant "c" expectedCallSite, expectedLiftedFn]
         in runPass lambdaLifting input `shouldBe` Right expectedModule

    describe "invariant: no ELam in any output object body" $ do
      it "lambda with no free vars is removed from the body" $
        let input = mkModule [DFunction Local "f" [] (ELam (ne [lbl "x"]) unit_)]
         in case runPass lambdaLifting input of
              Left err -> fail (show err)
              Right m ->
                concatMap
                  ( \case
                      DFunction Exported _ _ body -> checkLambdasLifted body
                      DConstant _ body -> checkLambdasLifted body
                      _ -> []
                  )
                  (moduleObjectsList m)
                  `shouldBe` []

      it "lambda with free vars is removed from the original body" $
        let input = mkModule [DFunction Local "f" [lbl "y"] (ELam (ne [lbl "x"]) (var "y"))]
         in case runPass lambdaLifting input of
              Left err -> fail (show err)
              Right m ->
                concatMap
                  ( \case
                      DFunction _ _ _ body -> checkLambdasLifted body
                      DConstant _ body -> checkLambdasLifted body
                      _ -> []
                  )
                  (moduleObjectsList m)
                  `shouldBe` []

    describe "generates additional top-level functions" $ do
      it "one lambda in input produces one extra DFunction in moduleObjects" $
        let input = mkModule [DFunction Local "f" [] (ELam (ne [lbl "x"]) unit_)]
         in case runPass lambdaLifting input of
              Left err -> fail (show err)
              Right m ->
                length (moduleObjectsList m) `shouldBe` 2

      it "two lambdas in input produce two extra DFunctions in moduleObjects" $
        -- Two lambdas: one in each object
        let input =
              mkModule
                [ DConstant "a" (ELam (ne [lbl "x"]) unit_)
                , DConstant "b" (ELam (ne [lbl "y"]) unit_)
                ]
         in case runPass lambdaLifting input of
              Left err -> fail (show err)
              Right m ->
                length (moduleObjectsList m) `shouldBe` 4

    describe "lifts self-recursive let-bound lambdas correctly" $ do
      it "let g = fn(t) => g(t) in g(unit): g excluded from captured free vars" $
        -- g references itself; after lifting:
        --   lam.0(t) = @<TOpq>(lam.0, t)   (recursive call via top-level name)
        --   g = lam.0                        (alias, no captured vars)
        let gType = TCon "/" [TOpq, TOpq] -- fn(TOpq) -> TOpq
            bodyExpr = EApp TOpq (var "g") (ne [var "t"]) -- body of fn(t) => g(t)
            letBody = EApp TOpq (var "g") (ne [unit_]) -- body of the enclosing let
            input =
              mkModule
                [ DConstant
                    "c"
                    (ELet (ne [Binding (lbl "g") (ELam (ne [lbl "t"]) bodyExpr)]) letBody)
                ]
            callSite = EVar (Label gType "lam.0")
            liftedBody = EApp TOpq callSite (ne [var "t"]) -- g substituted with callSite
            expectedModule =
              mkModule
                [ DConstant "c" (ELet (ne [Binding (lbl "g") callSite]) letBody)
                , DFunction Local "lam.0" [lbl "t"] liftedBody
                ]
         in runPass lambdaLifting input `shouldBe` Right expectedModule

      it "self-recursive lambda: invariant checkLambdasLifted passes" $
        let bodyExpr = EApp TOpq (var "g") (ne [var "t"])
            letBody = EApp TOpq (var "g") (ne [unit_])
            input =
              mkModule
                [ DConstant
                    "c"
                    (ELet (ne [Binding (lbl "g") (ELam (ne [lbl "t"]) bodyExpr)]) letBody)
                ]
         in case runPass lambdaLifting input of
              Left err -> fail (show err)
              Right m ->
                concatMap
                  ( \case
                      DFunction _ _ _ body -> checkLambdasLifted body
                      DConstant _ body -> checkLambdasLifted body
                      _ -> []
                  )
                  (moduleObjectsList m)
                  `shouldBe` []

-- helper (alias for consistency with local usage)
moduleObjectsList :: Module t -> [Object t]
moduleObjectsList = moduleObjects
