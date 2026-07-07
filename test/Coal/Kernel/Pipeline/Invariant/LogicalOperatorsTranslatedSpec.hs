{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Pipeline.Invariant.LogicalOperatorsTranslatedSpec (spec) where

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Coal.Kernel.Language.Op (Op (..))
import Coal.Kernel.Language.Prim (Prim (..))
import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Pipeline.Invariant.Error (InvariantError (..))
import Coal.Kernel.Pipeline.Invariant.LogicalOperatorsTranslated (checkLogicalOperatorsTranslated)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | A placeholder expression used as a body or operand in test cases.
unit_ :: Expr Type
unit_ = ELit PUnit

-- | Boolean literal @true@.
true_ :: Expr Type
true_ = ELit (PBool True)

-- | Boolean literal @false@.
false_ :: Expr Type
false_ = ELit (PBool False)

-- | Construct a label with an opaque type annotation.
lbl :: Text -> Label Type
lbl = Label TOpq

-- | Wrap an 'Op' node as an expression.
op_ :: Op (Expr Type) -> Expr Type
op_ = EOp

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "checkLogicalOperatorsTranslated" $ do
    describe "returns [] for valid expressions" $ do
      it "literal (no operators)" $
        checkLogicalOperatorsTranslated unit_
          `shouldBe` []

      it "if-then-else (the post-translation form of &&)" $
        checkLogicalOperatorsTranslated (EIf true_ false_ false_)
          `shouldBe` []

      it "if-then-else (the post-translation form of ||)" $
        checkLogicalOperatorsTranslated (EIf true_ true_ false_)
          `shouldBe` []

      it "unary NOT operator is not flagged" $
        checkLogicalOperatorsTranslated (op_ (ONot true_))
          `shouldBe` []

      it "arithmetic operator is not flagged" $
        checkLogicalOperatorsTranslated (op_ (OAddInt32 unit_ unit_))
          `shouldBe` []

      it "comparison operator is not flagged" $
        checkLogicalOperatorsTranslated (op_ (OEqBool true_ false_))
          `shouldBe` []

      it "let binding containing only if-then-else" $
        checkLogicalOperatorsTranslated
          (ELet (Binding (lbl "r") (EIf true_ false_ false_) :| []) unit_)
          `shouldBe` []

      it "case clause body containing only if-then-else" $
        checkLogicalOperatorsTranslated
          ( ECase
              TOpq
              unit_
              (Clause (lbl "Nothing" :| []) (EIf true_ true_ false_) :| [])
          )
          `shouldBe` []

    describe "returns errors for invalid expressions" $ do
      it "AND operator at the top level" $
        checkLogicalOperatorsTranslated (op_ (OAnd true_ false_))
          `shouldBe` [AndOperatorPresent]

      it "OR operator at the top level" $
        checkLogicalOperatorsTranslated (op_ (OOr true_ false_))
          `shouldBe` [OrOperatorPresent]

      it "AND and OR in the same expression each produce their own error" $
        checkLogicalOperatorsTranslated
          (op_ (OAnd (op_ (OOr true_ false_)) false_))
          `shouldBe` [AndOperatorPresent, OrOperatorPresent]

      it "AND buried inside a let binding's definition" $
        checkLogicalOperatorsTranslated
          (ELet (Binding (lbl "r") (op_ (OAnd true_ false_)) :| []) unit_)
          `shouldBe` [AndOperatorPresent]

      it "OR buried inside an if-then-else branch" $
        checkLogicalOperatorsTranslated
          (EIf true_ (op_ (OOr true_ false_)) false_)
          `shouldBe` [OrOperatorPresent]

      it "OR buried inside a case clause body" $
        checkLogicalOperatorsTranslated
          ( ECase
              TOpq
              unit_
              (Clause (lbl "Nothing" :| []) (op_ (OOr true_ false_)) :| [])
          )
          `shouldBe` [OrOperatorPresent]
