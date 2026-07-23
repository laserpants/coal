{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Pipeline.Invariant.LocalNamesUniqueSpec (spec) where

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Coal.Kernel.Language.Prim (Prim (..))
import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Pipeline.Invariant.Error (InvariantError (..))
import Coal.Kernel.Pipeline.Invariant.LocalNamesUnique (checkLocalNamesUnique)
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

-- | @let name = rhs in body@
letOne :: Text -> Expr Type -> Expr Type -> Expr Type
letOne name rhs body = ELet (Binding (lbl name) rhs :| []) body

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "checkLocalNamesUnique" $ do
    describe "returns [] for valid expressions" $ do
      it "literal (no binders at all)" $
        checkLocalNamesUnique unit_
          `shouldBe` []

      it "variable reference (not a binder)" $
        checkLocalNamesUnique (EVar (lbl "x"))
          `shouldBe` []

      it "constructor reference (not a binder)" $
        checkLocalNamesUnique (ECon (lbl "Just"))
          `shouldBe` []

      it "single let binding" $
        checkLocalNamesUnique (letOne "x" unit_ unit_)
          `shouldBe` []

      it "two nested lets with distinct names" $
        checkLocalNamesUnique (letOne "x" unit_ (letOne "y" unit_ unit_))
          `shouldBe` []

      it "lambda with two distinct parameters" $
        checkLocalNamesUnique (ELam (lbl "x" :| [lbl "y"]) unit_)
          `shouldBe` []

      it "case clauses with distinct pattern variables across all clauses" $
        checkLocalNamesUnique
          ( ECase
              TOpq
              unit_
              ( Clause (lbl "Cons" :| [lbl "head", lbl "tail"]) unit_
                  :| [Clause (lbl "Nil" :| []) unit_]
              )
          )
          `shouldBe` []

      it "nullary constructor in clause does not introduce a binder" $
        -- The head label of each Clause is the constructor name, not a pattern var
        checkLocalNamesUnique
          ( ECase
              TOpq
              unit_
              ( Clause (lbl "Nothing" :| []) unit_
                  :| [Clause (lbl "Just" :| [lbl "v"]) unit_]
              )
          )
          `shouldBe` []

    describe "returns errors for invalid expressions" $ do
      it "two nested lets with the same name" $
        checkLocalNamesUnique (letOne "x" unit_ (letOne "x" unit_ unit_))
          `shouldBe` [DuplicateLocalBinder "x"]

      it "lambda parameter shadows enclosing let binder" $
        checkLocalNamesUnique
          (letOne "x" unit_ (ELam (lbl "x" :| []) unit_))
          `shouldBe` [DuplicateLocalBinder "x"]

      it "duplicate lambda parameter names" $
        checkLocalNamesUnique (ELam (lbl "x" :| [lbl "x"]) unit_)
          `shouldBe` [DuplicateLocalBinder "x"]

      it "same pattern variable name in two clauses of the same case" $
        checkLocalNamesUnique
          ( ECase
              TOpq
              unit_
              ( Clause (lbl "Cons" :| [lbl "head", lbl "tail"]) unit_
                  :| [Clause (lbl "Pair" :| [lbl "head", lbl "x"]) unit_]
              )
          )
          `shouldBe` [DuplicateLocalBinder "head"]

      it "two distinct names each duplicated returns both errors (alphabetical)" $
        checkLocalNamesUnique
          ( letOne
              "x"
              unit_
              (letOne "y" unit_ (letOne "x" unit_ (letOne "y" unit_ unit_)))
          )
          `shouldBe` [DuplicateLocalBinder "x", DuplicateLocalBinder "y"]
