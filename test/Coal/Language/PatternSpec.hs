{-# LANGUAGE OverloadedStrings #-}

module Coal.Language.PatternSpec (patternBoundVarsSpec) where

import Coal.Common.FreeVars (BoundVars (..))
import Coal.Common.Label (Label (..))
import Coal.Common.Name (Name)
import Coal.Language.Pattern (Pattern (..))
import qualified Data.Set as Set
import Test.Hspec

patternBoundVarsSpec :: Spec
patternBoundVarsSpec =
  describe "BoundVars (Pattern)" $ do
    describe "boundIn" $ do
      it "binds only the @-variable of a fold pattern, not the fold name" $
        boundVars (PNamedFold () "delete_min_node" (Label () "left"))
          `shouldBe` Set.singleton "left"

      it "binds the @-variable of a bare recursive (@) pattern" $
        boundVars (PAtVariable () (Label () "rest"))
          `shouldBe` Set.singleton "rest"

      it "binds the variable of a plain variable pattern" $
        boundVars (PVariable () (Label () "x"))
          `shouldBe` Set.singleton "x"

      it "does not bind a constructor's own name" $
        boundVars (PConstructor () (Label () "Some") [PVariable () (Label () "v")])
          `shouldBe` Set.singleton "v"

      it "collects binders through list-cons, ignoring nested fold names" $
        boundVars
          ( PListCons
              ()
              ()
              (PVariable () (Label () "x"))
              (PNamedFold () "f" (Label () "rest"))
          )
          `shouldBe` Set.fromList ["x", "rest"]

-- | Type-restricted alias so the pattern literals below pin @s = ()@.
boundVars :: Pattern () () () -> Set.Set Name
boundVars = boundIn
