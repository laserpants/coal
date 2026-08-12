{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Pipeline.Pass.CaseExpressionCanonicalizationSpec (spec) where

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..))
import Coal.Kernel.Language.Module (Module (..), moduleObjects)
import Coal.Kernel.Language.Object (FunctionScope (..), Object (..))
import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Pipeline.Invariant (checkCaseExpressionsCanonical)
import Coal.Kernel.Pipeline.Pass.CaseExpressionCanonicalization (caseExpressionCanonicalization)
import Coal.Kernel.Pipeline.Pass.TestHelpers (lbl, mkModule, ne, runPass, unit_)
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Ord (comparing)
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | A nullary-constructor clause: @| ConName => unit_@.
leaf :: Text -> Clause Type
leaf name = Clause (ne [lbl name]) unit_

{- | Module with DData for the given (name, index) pairs and a single function
whose body is the given expression.

The indices are used to order the constructors in the source list, but the actual
DData representation stores them sorted lexicographically by name.
-}
modWithData :: [(Text, Int)] -> Expr Type -> Module Type
modWithData pairs e =
  let sortedPairs = sortBy (comparing fst) pairs -- Sort by name lexicographically
      ctors = [(name, TOpq) | (name, _) <- sortedPairs]
      dataObj = DData "TestType" ctors
   in mkModule [dataObj, DFunction Exported "f" [] e]

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "caseExpressionCanonicalization" $ do
    describe "does not change already-canonical expressions" $ do
      it "literal (no case)" $
        runPass caseExpressionCanonicalization (mkModule [DFunction Exported "f" [] unit_])
          `shouldBe` Right (mkModule [DFunction Exported "f" [] unit_])

      it "single-clause case (trivially sorted)" $
        let m = modWithData [("A", 0)] (ECase TOpq unit_ (ne [Clause (lbl "A" :| []) unit_]))
         in runPass caseExpressionCanonicalization m `shouldBe` Right m

      it "two clauses already in index order: A<0>, B<1>" $
        let m = modWithData [("A", 0), ("B", 1)] (ECase TOpq unit_ (ne [leaf "A", leaf "B"]))
         in runPass caseExpressionCanonicalization m `shouldBe` Right m

      it "three clauses already in index order: A<0>, B<1>, C<2>" $
        let m = modWithData [("A", 0), ("B", 1), ("C", 2)] (ECase TOpq unit_ (ne [leaf "A", leaf "B", leaf "C"]))
         in runPass caseExpressionCanonicalization m `shouldBe` Right m

      it "non-alphabetical but correct index order: Gt<0>, Lt<1>" $
        let m = modWithData [("Gt", 0), ("Lt", 1)] (ECase TOpq unit_ (ne [leaf "Gt", leaf "Lt"]))
         in runPass caseExpressionCanonicalization m `shouldBe` Right m

    describe "sorts clauses alphabetically by constructor name" $ do
      it "two constructors swapped alphabetically: input [B, A] — output [A, B]" $
        let input = modWithData [("B", 0), ("A", 1)] (ECase TOpq unit_ (ne [leaf "B", leaf "A"]))
            expected = modWithData [("B", 0), ("A", 1)] (ECase TOpq unit_ (ne [leaf "A", leaf "B"]))
         in runPass caseExpressionCanonicalization input `shouldBe` Right expected

      it "three constructors reversed alphabetically: input [C, B, A] — output [A, B, C]" $
        let input = modWithData [("A", 2), ("B", 1), ("C", 0)] (ECase TOpq unit_ (ne [leaf "C", leaf "B", leaf "A"]))
            expected = modWithData [("A", 2), ("B", 1), ("C", 0)] (ECase TOpq unit_ (ne [leaf "A", leaf "B", leaf "C"]))
         in runPass caseExpressionCanonicalization input `shouldBe` Right expected

      it "alphabetical sort: Eq, Gt, Lt — input [Lt, Eq, Gt], output [Eq, Gt, Lt]" $
        let input =
              modWithData
                [("Lt", 1), ("Gt", 2), ("Eq", 0)]
                (ECase TOpq unit_ (ne [leaf "Lt", leaf "Eq", leaf "Gt"]))
            expected =
              modWithData
                [("Lt", 1), ("Gt", 2), ("Eq", 0)]
                (ECase TOpq unit_ (ne [leaf "Eq", leaf "Gt", leaf "Lt"]))
         in runPass caseExpressionCanonicalization input `shouldBe` Right expected

      it "case nested inside a let binding body is also sorted" $
        let inner clauses = ECase TOpq unit_ (ne (map leaf clauses))
            input =
              modWithData
                [("A", 0), ("Z", 1)]
                (ELet (ne [Binding (lbl "r") (inner ["Z", "A"])]) unit_)
            expected =
              modWithData
                [("A", 0), ("Z", 1)]
                (ELet (ne [Binding (lbl "r") (inner ["A", "Z"])]) unit_)
         in runPass caseExpressionCanonicalization input `shouldBe` Right expected

    -- NOTE: Index validation tests removed since indices are now implicit from
    -- lexicographic position in the grouped DData representation, so they are
    -- always valid by construction.

    describe "invariant: checkCaseExpressionsCanonical [] on every object body" $ do
      it "after sorting a reversed two-clause case" $
        let input =
              modWithData
                [("A", 0), ("Z", 1)]
                (ECase TOpq unit_ (ne [leaf "Z", leaf "A"]))
         in case runPass caseExpressionCanonicalization input of
              Left err -> fail (show err)
              Right m ->
                concatMap
                  ( \case
                      DFunction _ _ _ body -> checkCaseExpressionsCanonical body
                      DConstant _ body -> checkCaseExpressionsCanonical body
                      _ -> []
                  )
                  (moduleObjects m)
                  `shouldBe` []

    describe "idempotence" $ do
      it "running the pass twice gives the same result as running it once" $
        let input =
              modWithData
                [("A", 0), ("Z", 1)]
                (ECase TOpq unit_ (ne [leaf "Z", leaf "A"]))
         in do
              let once = runPass caseExpressionCanonicalization input
              case once of
                Left e -> fail (show e)
                Right m1 ->
                  runPass caseExpressionCanonicalization m1 `shouldBe` Right m1
