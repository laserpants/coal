{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Pipeline.Pass.FunctionResultsSaturationSpec (spec) where

import Coal.Kernel.Language.Expr (Expr (..), Label (..))
import Coal.Kernel.Language.Module (Module (..), moduleObjects)
import Coal.Kernel.Language.Object (FunctionScope (..), Object (..))
import Coal.Kernel.Language.Prim (Prim (..))
import Coal.Kernel.Language.Type (Type (..))
import qualified Coal.Kernel.Language.Type.Constructors as Type
import Coal.Kernel.Language.Type.Function ((~>))
import Coal.Kernel.Pipeline.Invariant (checkFunctionResultsSaturated)
import Coal.Kernel.Pipeline.Pass.FunctionResultsSaturation (functionResultsSaturation)
import Coal.Kernel.Pipeline.Pass.TestHelpers (lbl, mkModule, runPass, unit_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

ne :: [a] -> NonEmpty a
ne (x : xs) = x :| xs
ne [] = error "ne: empty list"

-- | Variable reference with int32 type.
ivar :: Text -> Expr Type
ivar n = EVar (Label Type.int32 n)

-- | Variable reference with bool type.
bvar :: Text -> Expr Type
bvar n = EVar (Label Type.bool n)

{- | A body expression whose type is @int32 ~> bool@ (a function type).
We use a variable whose label has this type.
-}
fnBody :: Expr Type
fnBody = EVar (Label (Type.int32 ~> Type.bool) "body")

-- | A body whose type is @int32 ~> int32 ~> bool@ (two-arg function type).
fn2Body :: Expr Type
fn2Body = EVar (Label (Type.int32 ~> Type.int32 ~> Type.bool) "body2")

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "functionResultsSaturation" $ do
    describe "leaves non-function-result objects unchanged" $ do
      it "function returning int32 (not a function type): unchanged" $
        let m = mkModule [DFunction Exported "f" [lbl "x"] (ELit (PInt32 0))]
         in runPass functionResultsSaturation m `shouldBe` Right m

      it "function returning unit (TOpq): unchanged" $
        let m = mkModule [DFunction Exported "f" [lbl "x"] unit_]
         in runPass functionResultsSaturation m `shouldBe` Right m

      it "DConstant with non-function body is unchanged" $
        let m = mkModule [DConstant "c" unit_]
         in runPass functionResultsSaturation m `shouldBe` Right m

      it "DExternal is unchanged" $
        let m = mkModule [DExternal "e" TOpq]
         in runPass functionResultsSaturation m `shouldBe` Right m

    describe "promotes function-typed DConstant to DFunction" $ do
      it "DConstant whose body has type int32->bool  =>  DFunction [r.0] body(r.0)" $
        -- fnBody :: Expr Type  has typeOf = int32 ~> bool
        -- After saturation: promoted to DFunction with one fresh param
        let freshParam = Label Type.int32 "r.0"
            input = mkModule [DConstant "f2" fnBody]
            expected =
              mkModule
                [ DFunction
                    Exported
                    "f2"
                    [freshParam]
                    (EApp Type.bool fnBody (ne [EVar freshParam]))
                ]
         in runPass functionResultsSaturation input `shouldBe` Right expected

      it "DConstant whose body has type int32->int32->bool  =>  DFunction [r.0, r.1] body(r.0, r.1)" $
        let r0 = Label Type.int32 "r.0"
            r1 = Label Type.int32 "r.1"
            newBody = EApp Type.bool fn2Body (ne [EVar r0, EVar r1])
            input = mkModule [DConstant "f2" fn2Body]
            expected = mkModule [DFunction Exported "f2" [r0, r1] newBody]
         in runPass functionResultsSaturation input `shouldBe` Right expected

      it "after promoting a function-typed DConstant, checkFunctionResultsSaturated returns []" $
        let input = mkModule [DConstant "f2" fnBody]
         in case runPass functionResultsSaturation input of
              Left err -> fail (show err)
              Right m ->
                concatMap checkFunctionResultsSaturated (moduleObjectsList m)
                  `shouldBe` []

    describe "eta-expands function-result functions" $ do
      it "function f(x) whose body has type int32->bool  =>  f(x, r.0) with body(r.0)" $
        -- fnBody :: Expr Type  has typeOf = int32 ~> bool
        -- After saturation: add param r.0 : int32, body becomes EApp bool fnBody [EVar r.0]
        let freshParam = Label Type.int32 "r.0"
            input = mkModule [DFunction Exported "f" [lbl "x"] fnBody]
            expected =
              mkModule
                [ DFunction
                    Exported
                    "f"
                    [lbl "x", freshParam]
                    (EApp Type.bool fnBody (ne [EVar freshParam]))
                ]
         in runPass functionResultsSaturation input `shouldBe` Right expected

      it "function f() whose body has type int32->int32->bool  =>  f(r.0, r.1) with body(r.0, r.1)" $
        -- fn2Body has type int32 ~> int32 ~> bool
        -- unfoldType gives [int32, int32, bool]
        -- argTypes = [int32, int32], so fresh params are [r.0, r.1]
        -- body' = EApp bool fn2Body (ne [EVar r.0, EVar r.1])  (flat, not curried)
        let r0 = Label Type.int32 "r.0"
            r1 = Label Type.int32 "r.1"
            newBody = EApp Type.bool fn2Body (ne [EVar r0, EVar r1])
            input = mkModule [DFunction Exported "f" [] fn2Body]
            expected = mkModule [DFunction Exported "f" [r0, r1] newBody]
         in runPass functionResultsSaturation input `shouldBe` Right expected

    describe "invariant: checkFunctionResultsSaturated [] on every object" $ do
      it "after saturating a single-arrow result function" $
        let input = mkModule [DFunction Exported "f" [lbl "x"] fnBody]
         in case runPass functionResultsSaturation input of
              Left err -> fail (show err)
              Right m ->
                concatMap checkFunctionResultsSaturated (moduleObjectsList m)
                  `shouldBe` []

      it "after saturating a double-arrow result function" $
        let input = mkModule [DFunction Exported "f" [] fn2Body]
         in case runPass functionResultsSaturation input of
              Left err -> fail (show err)
              Right m ->
                concatMap checkFunctionResultsSaturated (moduleObjectsList m)
                  `shouldBe` []

-- helpers
moduleObjectsList :: Module t -> [Object t]
moduleObjectsList = moduleObjects
