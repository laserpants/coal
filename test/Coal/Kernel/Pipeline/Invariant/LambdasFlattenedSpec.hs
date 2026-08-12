{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Pipeline.Invariant.LambdasFlattenedSpec (spec) where

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Coal.Kernel.Language.Prim (Prim (..))
import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Pipeline.Invariant.Error (InvariantError (..))
import Coal.Kernel.Pipeline.Invariant.LambdasFlattened (checkLambdasFlattened)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | A placeholder expression used as a body or scrutinee in test cases.
unit_ :: Expr Type
unit_ = ELit PUnit

-- | Construct a label with an opaque type annotation.
lbl :: Text -> Label Type
lbl = Label TOpq

-- | @fn(p) => body@ — a single-parameter lambda.
lam1 :: Text -> Expr Type -> Expr Type
lam1 p = ELam (lbl p :| [])

-- | @fn(p, q) => body@ — a two-parameter (already-flat) lambda.
lam2 :: Text -> Text -> Expr Type -> Expr Type
lam2 p q = ELam (lbl p :| [lbl q])

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "checkLambdasFlattened" $ do
    describe "returns [] for valid expressions" $ do
      it "literal (no lambdas)" $
        checkLambdasFlattened unit_
          `shouldBe` []

      it "flat two-parameter lambda" $
        checkLambdasFlattened (lam2 "x" "y" unit_)
          `shouldBe` []

      it "lambda whose body is a let expression" $
        checkLambdasFlattened
          (lam1 "x" (ELet (Binding (lbl "r") unit_ :| []) unit_))
          `shouldBe` []

      it "lambda whose body is a case expression" $
        checkLambdasFlattened
          ( lam1
              "x"
              (ECase TOpq unit_ (Clause (lbl "Nothing" :| []) unit_ :| []))
          )
          `shouldBe` []

      it "lambda whose body is an if expression" $
        checkLambdasFlattened (lam1 "x" (EIf unit_ unit_ unit_))
          `shouldBe` []

      it "lambda stored in a let binding (body is not itself a lambda)" $
        checkLambdasFlattened
          (ELet (Binding (lbl "f") (lam2 "a" "b" unit_) :| []) unit_)
          `shouldBe` []

    describe "returns errors for invalid expressions" $ do
      it "one level of nesting: fn(x) => fn(y) => unit" $
        checkLambdasFlattened (lam1 "x" (lam1 "y" unit_))
          `shouldBe` [NestedLambdaBody]

      it "two levels of nesting yields two errors: fn(x) => fn(y) => fn(z) => unit" $
        -- Outer fn detects its body is a fn (first error).
        -- Recursing into that body, the middle fn also detects its body is a fn
        -- (second error). The innermost fn's body is a literal — no further error.
        checkLambdasFlattened (lam1 "x" (lam1 "y" (lam1 "z" unit_)))
          `shouldBe` [NestedLambdaBody, NestedLambdaBody]

      it "nested lambda in a let binding's definition" $
        checkLambdasFlattened
          (ELet (Binding (lbl "f") (lam1 "x" (lam1 "y" unit_)) :| []) unit_)
          `shouldBe` [NestedLambdaBody]

      it "nested lambda in a case clause body" $
        checkLambdasFlattened
          ( ECase
              TOpq
              unit_
              (Clause (lbl "Nothing" :| []) (lam1 "x" (lam1 "y" unit_)) :| [])
          )
          `shouldBe` [NestedLambdaBody]

      it "nested lambda in the else branch of an if expression" $
        checkLambdasFlattened (EIf unit_ unit_ (lam1 "x" (lam1 "y" unit_)))
          `shouldBe` [NestedLambdaBody]
