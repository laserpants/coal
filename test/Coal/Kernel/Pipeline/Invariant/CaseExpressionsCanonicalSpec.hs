{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Pipeline.Invariant.CaseExpressionsCanonicalSpec (spec) where

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Coal.Kernel.Language.Prim (Prim (..))
import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Pipeline.Invariant.CaseExpressionsCanonical (checkCaseExpressionsCanonical)
import Coal.Kernel.Pipeline.Invariant.Error (InvariantError (..))
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

-- | A nullary-constructor clause: @| ConName => unit_@
leafClause :: Text -> Clause Type
leafClause conName = Clause (lbl conName :| []) unit_

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "checkCaseExpressionsCanonical" $ do
    describe "returns [] for valid expressions" $ do
      it "literal (no case expressions)" $
        checkCaseExpressionsCanonical unit_
          `shouldBe` []

      it "single-clause case (no pairs to compare)" $
        checkCaseExpressionsCanonical
          (ECase TOpq unit_ (leafClause "Nothing" :| []))
          `shouldBe` []

      it "two clauses in ascending alphabetical order: A then B" $
        checkCaseExpressionsCanonical
          (ECase TOpq unit_ (leafClause "A" :| [leafClause "B"]))
          `shouldBe` []

      it "three clauses in ascending alphabetical order: A, B, C" $
        checkCaseExpressionsCanonical
          ( ECase
              TOpq
              unit_
              (leafClause "A" :| [leafClause "B", leafClause "C"])
          )
          `shouldBe` []

      it "Gt before Lt — already in alphabetical order (G < L)" $
        checkCaseExpressionsCanonical
          (ECase TOpq unit_ (leafClause "Gt" :| [leafClause "Lt"]))
          `shouldBe` []

      it "clause with pattern-bound variables: Cons, Nil — alphabetical order" $
        checkCaseExpressionsCanonical
          ( ECase
              TOpq
              unit_
              ( Clause (lbl "Cons" :| [lbl "h", lbl "t"]) unit_
                  :| [Clause (lbl "Nil" :| []) unit_]
              )
          )
          `shouldBe` []

      it "unknown or imported constructors are always compared by name" $
        checkCaseExpressionsCanonical
          (ECase TOpq unit_ (leafClause "A" :| [leafClause "Z"]))
          `shouldBe` []

      it "valid case nested inside a let binding's definition" $
        checkCaseExpressionsCanonical
          ( ELet
              ( Binding
                  (lbl "r")
                  (ECase TOpq unit_ (leafClause "A" :| [leafClause "B"]))
                  :| []
              )
              unit_
          )
          `shouldBe` []

    describe "returns errors for invalid expressions" $ do
      it "two clauses out of order alphabetically: B before A" $
        checkCaseExpressionsCanonical
          (ECase TOpq unit_ (leafClause "B" :| [leafClause "A"]))
          `shouldBe` [CaseClausesOutOfOrder "B" "A"]

      it "Lt before Gt is out of alphabetical order (L > G)" $
        checkCaseExpressionsCanonical
          (ECase TOpq unit_ (leafClause "Lt" :| [leafClause "Gt"]))
          `shouldBe` [CaseClausesOutOfOrder "Lt" "Gt"]

      it "three clauses with one violation: A, C, B — C before B is wrong" $
        checkCaseExpressionsCanonical
          ( ECase
              TOpq
              unit_
              (leafClause "A" :| [leafClause "C", leafClause "B"])
          )
          `shouldBe` [CaseClausesOutOfOrder "C" "B"]

      it "three fully-reversed clauses yields two violations: C, B, A" $
        checkCaseExpressionsCanonical
          ( ECase
              TOpq
              unit_
              (leafClause "C" :| [leafClause "B", leafClause "A"])
          )
          `shouldBe` [ CaseClausesOutOfOrder "C" "B"
                     , CaseClausesOutOfOrder "B" "A"
                     ]

      it "violation nested inside a clause body" $
        checkCaseExpressionsCanonical
          ( ECase
              TOpq
              unit_
              ( Clause
                  (lbl "X" :| [])
                  (ECase TOpq unit_ (leafClause "B" :| [leafClause "A"]))
                  :| []
              )
          )
          `shouldBe` [CaseClausesOutOfOrder "B" "A"]

      it "violation inside the scrutinee expression" $
        checkCaseExpressionsCanonical
          ( ECase
              TOpq
              (ECase TOpq unit_ (leafClause "B" :| [leafClause "A"]))
              (leafClause "X" :| [])
          )
          `shouldBe` [CaseClausesOutOfOrder "B" "A"]
