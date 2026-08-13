{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.TypeCheckSpec (spec) where

import Coal.Kernel.Language.Expr (Expr (..), Label (..))
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (FunctionScope (..), Object (..))
import Coal.Kernel.Language.Prim (Prim (..))
import Coal.Kernel.Language.Type (Type (..))
import qualified Coal.Kernel.Language.Type.Constructors as Type
import Coal.Kernel.TypeCheck (checkModules)
import Coal.Kernel.TypeCheck.Error (Context (..), TypeError (..), TypeErrorKind (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)

-- | A label with the given type.
lbl :: Type -> Text -> Label Type
lbl = Label

-- | A variable reference with the given type.
var :: Type -> Text -> Expr Type
var t n = EVar (Label t n)

-- | A module containing a single constant @bad@ whose body is the given expression.
modWith :: Expr Type -> Module Type
modWith e = Module "Test" [] [DConstant "bad" e]

spec :: Spec
spec = do
  describe "checkModules" $ do
    it "accepts a well-typed identity function" $ do
      let m = Module "Test" [] [DFunction Exported "id" [lbl Type.int32 "x"] (var Type.int32 "x")]
      checkModules [m] `shouldBe` []

    it "accepts a well-typed constant" $ do
      let m = Module "Test" [] [DConstant "answer" (ELit (PInt32 42))]
      checkModules [m] `shouldBe` []

    it "reports a type mismatch when applying an int32 function to a bool" $ do
      let f = var (Type.arrow Type.int32 Type.int32) "f"
          bad = EApp Type.int32 f (var Type.bool "b" :| [])
      checkModules [modWith bad]
        `shouldBe` [TypeError (InObject "bad") (TypeMismatch Type.int32 Type.bool)]

    it "reports an arity mismatch when too many arguments are applied" $ do
      let f = var (Type.arrow Type.int32 Type.int32) "f"
          bad = EApp Type.int32 f (var Type.int32 "a" :| [var Type.int32 "b"])
      checkModules [modWith bad]
        `shouldBe` [TypeError (InObject "bad") (ArityMismatch 1 2)]

    it "reports a condition-not-bool error for a non-bool if condition" $ do
      let bad = EIf (ELit (PInt32 1)) (var Type.int32 "x") (var Type.int32 "y")
      checkModules [modWith bad]
        `shouldBe` [TypeError (InObject "bad") (ConditionNotBool Type.int32)]

    it "reports a branch type mismatch for mismatched if branches" $ do
      let bad = EIf (ELit (PBool True)) (var Type.int32 "x") (var Type.bool "y")
      checkModules [modWith bad]
        `shouldBe` [TypeError (InObject "bad") (BranchTypeMismatch Type.int32 Type.bool)]
