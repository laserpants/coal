{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Pipeline.Pass.LogicalOperatorTranslationSpec (spec) where

import Coal.Kernel.Language.Expr (Binding (..), Expr (..))
import Coal.Kernel.Language.Module (Module (..), moduleObjects)
import Coal.Kernel.Language.Object (Object (..))
import Coal.Kernel.Language.Op (Op (..))
import Coal.Kernel.Language.Prim (Prim (..))
import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Pipeline.Invariant (checkLogicalOperatorsTranslated)
import Coal.Kernel.Pipeline.Pass.LogicalOperatorTranslation (logicalOperatorTranslation)
import Coal.Kernel.Pipeline.Pass.TestHelpers (lbl, mkModule, ne, runPass, unit_)
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Boolean variable reference.
bvar :: Text -> Expr Type
bvar name = EVar (lbl name)

-- | @a && b@
andE :: Expr Type -> Expr Type -> Expr Type
andE a b = EOp (OAnd a b)

-- | @a || b@
orE :: Expr Type -> Expr Type -> Expr Type
orE a b = EOp (OOr a b)

-- | Boolean literal.
bool :: Bool -> Expr Type
bool = ELit . PBool

-- | Module with a single zero-param constant.
modWith :: Expr Type -> Module Type
modWith e = mkModule [DConstant "x" e]

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "logicalOperatorTranslation" $ do
    describe "translates && to if-then-else" $ do
      it "a && b  =>  if a then b else false" $
        let a = bvar "a"
            b = bvar "b"
            input = modWith (andE a b)
            expected = modWith (EIf a b (bool False))
         in runPass logicalOperatorTranslation input `shouldBe` Right expected

      it "a && b where a is itself a complex expression" $
        let a = EIf (bvar "c") (bvar "d") (bool False)
            b = bvar "b"
            input = modWith (andE a b)
            expected = modWith (EIf a b (bool False))
         in runPass logicalOperatorTranslation input `shouldBe` Right expected

    describe "translates || to if-then-else" $ do
      it "a || b  =>  if a then true else b" $
        let a = bvar "a"
            b = bvar "b"
            input = modWith (orE a b)
            expected = modWith (EIf a (bool True) b)
         in runPass logicalOperatorTranslation input `shouldBe` Right expected

    describe "translates nested logical operators" $ do
      it "(a && b) || c  =>  if (if a then b else false) then true else c" $
        let a = bvar "a"
            b = bvar "b"
            c = bvar "c"
            input = modWith (orE (andE a b) c)
            expected = modWith (EIf (EIf a b (bool False)) (bool True) c)
         in runPass logicalOperatorTranslation input `shouldBe` Right expected

      it "a && (b || c)  =>  if a then (if b then true else c) else false" $
        let a = bvar "a"
            b = bvar "b"
            c = bvar "c"
            input = modWith (andE a (orE b c))
            expected = modWith (EIf a (EIf b (bool True) c) (bool False))
         in runPass logicalOperatorTranslation input `shouldBe` Right expected

    describe "leaves other operators unchanged" $ do
      it "OAddInt32 operands are recursed but the op remains" $
        let e = EOp (OAddInt32 (bvar "x") (bvar "y"))
            m = modWith e
         in runPass logicalOperatorTranslation m `shouldBe` Right m

      it "ONot operand is recursed (a && b inside becomes if-form)" $
        let a = bvar "a"
            b = bvar "b"
            input = modWith (EOp (ONot (andE a b)))
            expected = modWith (EOp (ONot (EIf a b (bool False))))
         in runPass logicalOperatorTranslation input `shouldBe` Right expected

      it "logical operator inside let RHS also translated" $
        let a = bvar "a"
            b = bvar "b"
            input = modWith (ELet (ne [Binding (lbl "r") (andE a b)]) unit_)
            expected = modWith (ELet (ne [Binding (lbl "r") (EIf a b (bool False))]) unit_)
         in runPass logicalOperatorTranslation input `shouldBe` Right expected

    describe "invariant: checkLogicalOperatorsTranslated [] on every object body" $ do
      it "after translating a && b" $
        let input = mkModule [DConstant "x" (andE (bvar "a") (bvar "b"))]
         in case runPass logicalOperatorTranslation input of
              Left err -> fail (show err)
              Right m ->
                concatMap
                  ( \case
                      DFunction _ _ _ body -> checkLogicalOperatorsTranslated body
                      DConstant _ body -> checkLogicalOperatorsTranslated body
                      _ -> []
                  )
                  (moduleObjects m)
                  `shouldBe` []

      it "after translating nested operators" $
        let input = mkModule [DConstant "x" (orE (andE (bvar "a") (bvar "b")) (bvar "c"))]
         in case runPass logicalOperatorTranslation input of
              Left err -> fail (show err)
              Right m ->
                concatMap
                  ( \case
                      DFunction _ _ _ body -> checkLogicalOperatorsTranslated body
                      DConstant _ body -> checkLogicalOperatorsTranslated body
                      _ -> []
                  )
                  (moduleObjects m)
                  `shouldBe` []
