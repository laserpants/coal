{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Pipeline.Invariant.LambdasLiftedSpec (spec) where

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Coal.Kernel.Language.Op (Op (..))
import Coal.Kernel.Language.Prim (Prim (..))
import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Pipeline.Invariant.Error (InvariantError (..))
import Coal.Kernel.Pipeline.Invariant.LambdasLifted (checkLambdasLifted)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | A placeholder expression.
unit_ :: Expr Type
unit_ = ELit PUnit

-- | Construct a label with an opaque type annotation.
lbl :: Text -> Label Type
lbl = Label TOpq

-- | @let name = rhs in body@
letOne :: Text -> Expr Type -> Expr Type -> Expr Type
letOne name rhs = ELet (Binding (lbl name) rhs :| [])

-- | @fn(p) => body@
lam1 :: Text -> Expr Type -> Expr Type
lam1 p = ELam (lbl p :| [])

-- | @fn(p, q) => body@
lam2 :: Text -> Text -> Expr Type -> Expr Type
lam2 p q = ELam (lbl p :| [lbl q])

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "checkLambdasLifted" $ do
    describe "returns [] for valid expressions (no lambdas)" $ do
      it "literal (no lambda)" $
        checkLambdasLifted unit_
          `shouldBe` []

      it "variable reference (no lambda)" $
        checkLambdasLifted (EVar (lbl "x"))
          `shouldBe` []

      it "constructor reference (no lambda)" $
        checkLambdasLifted (ECon (lbl "Just"))
          `shouldBe` []

      it "application with no lambdas" $
        checkLambdasLifted
          (EApp TOpq (EVar (lbl "f")) (EVar (lbl "x") :| [EVar (lbl "y")]))
          `shouldBe` []

      it "let binding with no lambdas" $
        checkLambdasLifted (letOne "x" unit_ (EVar (lbl "x")))
          `shouldBe` []

      it "if-then-else with no lambdas" $
        checkLambdasLifted (EIf (EVar (lbl "c")) (EVar (lbl "t")) (EVar (lbl "f")))
          `shouldBe` []

      it "case expression with no lambdas" $
        checkLambdasLifted
          ( ECase
              TOpq
              (EVar (lbl "xs"))
              ( Clause (lbl "Cons" :| [lbl "x", lbl "xs"]) (EVar (lbl "x"))
                  :| [Clause (lbl "Nil" :| []) unit_]
              )
          )
          `shouldBe` []

      it "operator with no lambdas" $
        checkLambdasLifted (EOp (OAddInt32 (EVar (lbl "x")) (EVar (lbl "y"))))
          `shouldBe` []

      it "record extension with no lambdas" $
        checkLambdasLifted (EExt "field" (EVar (lbl "x")) (EVar (lbl "y")))
          `shouldBe` []

      it "record projection with no lambdas" $
        checkLambdasLifted (EGet (lbl "field") (EVar (lbl "r")))
          `shouldBe` []

      it "empty record" $
        checkLambdasLifted (ENil :: Expr Type)
          `shouldBe` []

    describe "returns errors for invalid expressions (lambdas present)" $ do
      it "lambda at top level" $
        checkLambdasLifted (lam1 "x" unit_)
          `shouldBe` [LambdaNotLifted]

      it "multi-parameter lambda at top level" $
        checkLambdasLifted (lam2 "x" "y" unit_)
          `shouldBe` [LambdaNotLifted]

      it "lambda inside let binding definition" $
        checkLambdasLifted (letOne "f" (lam1 "x" unit_) unit_)
          `shouldBe` [LambdaNotLifted]

      it "lambda inside let binding body" $
        checkLambdasLifted (letOne "x" unit_ (lam1 "y" unit_))
          `shouldBe` [LambdaNotLifted]

      it "lambda inside if condition" $
        checkLambdasLifted (EIf (lam1 "x" unit_) unit_ unit_)
          `shouldBe` [LambdaNotLifted]

      it "lambda inside if then-branch" $
        checkLambdasLifted (EIf unit_ (lam1 "x" unit_) unit_)
          `shouldBe` [LambdaNotLifted]

      it "lambda inside if else-branch" $
        checkLambdasLifted (EIf unit_ unit_ (lam1 "x" unit_))
          `shouldBe` [LambdaNotLifted]

      it "lambda inside case scrutinee" $
        checkLambdasLifted
          ( ECase
              TOpq
              (lam1 "x" unit_)
              (Clause (lbl "Nothing" :| []) unit_ :| [])
          )
          `shouldBe` [LambdaNotLifted]

      it "lambda inside case clause body" $
        checkLambdasLifted
          ( ECase
              TOpq
              unit_
              (Clause (lbl "Nothing" :| []) (lam1 "x" unit_) :| [])
          )
          `shouldBe` [LambdaNotLifted]

      it "lambda inside function position of application" $
        checkLambdasLifted
          (EApp TOpq (lam1 "x" unit_) (unit_ :| []))
          `shouldBe` [LambdaNotLifted]

      it "lambda inside argument of application" $
        checkLambdasLifted
          (EApp TOpq (EVar (lbl "f")) (lam1 "x" unit_ :| []))
          `shouldBe` [LambdaNotLifted]

      it "lambda inside operator operand" $
        checkLambdasLifted (EOp (OAddInt32 (lam1 "x" unit_) unit_))
          `shouldBe` [LambdaNotLifted]

      it "lambda inside record extension first expression" $
        checkLambdasLifted (EExt "field" (lam1 "x" unit_) unit_)
          `shouldBe` [LambdaNotLifted]

      it "lambda inside record extension second expression" $
        checkLambdasLifted (EExt "field" unit_ (lam1 "x" unit_))
          `shouldBe` [LambdaNotLifted]

      it "lambda inside record projection" $
        checkLambdasLifted (EGet (lbl "field") (lam1 "x" unit_))
          `shouldBe` [LambdaNotLifted]

      it "multiple lambdas in different locations report multiple errors" $
        checkLambdasLifted
          ( EIf
              (lam1 "a" unit_)
              (lam1 "b" unit_)
              (lam1 "c" unit_)
          )
          `shouldBe` [LambdaNotLifted, LambdaNotLifted, LambdaNotLifted]

      it "nested let with lambda in inner binding" $
        checkLambdasLifted
          ( letOne
              "x"
              unit_
              (letOne "f" (lam1 "y" unit_) unit_)
          )
          `shouldBe` [LambdaNotLifted]
