{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Pipeline.Invariant.TopLevelFunctionsNormalizedSpec (spec) where

import Coal.Kernel.Language.Expr (Binding (..), Expr (..), Label (..))
import Coal.Kernel.Language.Object (FunctionScope (..), Object (..))
import Coal.Kernel.Language.Prim (Prim (..))
import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Pipeline.Invariant.Error (InvariantError (..))
import Coal.Kernel.Pipeline.Invariant.TopLevelFunctionsNormalized (checkTopLevelFunctionsNormalized)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | A placeholder expression used as a body or initialiser in test cases.
unit_ :: Expr Type
unit_ = ELit PUnit

-- | Construct a label with an opaque type annotation.
lbl :: Text -> Label Type
lbl = Label TOpq

-- | @fn(p) => body@ — a single-parameter lambda.
lam1 :: Text -> Expr Type -> Expr Type
lam1 p = ELam (lbl p :| [])

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "checkTopLevelFunctionsNormalized" $ do
    describe "returns [] for valid objects" $ do
      it "function with a non-lambda body" $
        checkTopLevelFunctionsNormalized
          (DFunction Exported "foo" [lbl "x"] unit_)
          `shouldBe` []

      it "parameterless function with a non-lambda body" $
        checkTopLevelFunctionsNormalized
          (DFunction Exported "foo" [] unit_)
          `shouldBe` []

      it "constant whose expression is a literal" $
        checkTopLevelFunctionsNormalized
          (DConstant "bar" unit_)
          `shouldBe` []

      it "constant whose expression is a let (not a lambda)" $
        checkTopLevelFunctionsNormalized
          (DConstant "bar" (ELet (Binding (lbl "r") unit_ :| []) unit_))
          `shouldBe` []

      it "external declaration" $
        checkTopLevelFunctionsNormalized
          (DExternal "ext" TOpq)
          `shouldBe` []

      it "data type declaration" $
        checkTopLevelFunctionsNormalized
          (DData "Maybe" [("Maybe", TOpq)])
          `shouldBe` []

    describe "returns errors for invalid objects" $ do
      it "constant whose expression is directly a lambda" $
        checkTopLevelFunctionsNormalized
          (DConstant "foo" (lam1 "x" unit_))
          `shouldBe` [ConstantContainsLambda "foo"]

      it "parameterless function whose body is directly a lambda" $
        checkTopLevelFunctionsNormalized
          (DFunction Exported "foo" [] (lam1 "y" unit_))
          `shouldBe` [FunctionBodyIsLambda "foo"]

      it "function with existing params whose body is still directly a lambda" $
        checkTopLevelFunctionsNormalized
          (DFunction Exported "foo" [lbl "x"] (lam1 "y" unit_))
          `shouldBe` [FunctionBodyIsLambda "foo"]
