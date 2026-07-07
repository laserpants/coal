{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Pipeline.Pass.ConstructorSaturationSpec (spec) where

import Coal.Kernel.Language.Expr (Expr (..), Label (..))
import Coal.Kernel.Language.Module (Module (..), moduleObjects)
import Coal.Kernel.Language.Object (FunctionScope (..), Object (..))
import Coal.Kernel.Language.Type (Type (..))
import qualified Coal.Kernel.Language.Type.Constructors as Type
import Coal.Kernel.Language.Type.Function ((~>))
import Coal.Kernel.Pipeline (PipelineError (..))
import Coal.Kernel.Pipeline.Invariant (checkConstructorsSaturated)
import Coal.Kernel.Pipeline.Pass.ConstructorSaturation (constructorSaturation)
import Coal.Kernel.Pipeline.Pass.TestHelpers (mkModule, runPass)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

ne :: [a] -> NonEmpty a
ne (x : xs) = x :| xs
ne [] = error "ne: empty list"

-- | Constructor label with the given full type.
conLbl :: Text -> Type -> Label Type
conLbl name t = Label t name

-- | A nullary constructor type.
nullaryConType :: Type
nullaryConType = Type.bool -- e.g. True : bool, arity = 0

-- | An arity-1 constructor type: int32 -> bool.
arity1ConType :: Type
arity1ConType = Type.int32 ~> Type.bool

-- | An arity-2 constructor type: int32 -> int32 -> bool.
arity2ConType :: Type
arity2ConType = Type.int32 ~> Type.int32 ~> Type.bool

-- | Bare constructor expression (no arguments).
bareECon :: Text -> Type -> Expr Type
bareECon name t = ECon (Label t name)

-- | Variable reference with int32 type.
ivar :: Text -> Expr Type
ivar n = EVar (Label Type.int32 n)

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "constructorSaturation" $ do
    describe "leaves already-saturated constructors unchanged" $ do
      it "nullary constructor (arity 0): ECon unchanged" $
        let m = mkModule [DConstant "c" (bareECon "True" nullaryConType)]
         in runPass constructorSaturation m `shouldBe` Right m

      it "arity-1 constructor applied to exactly 1 argument: unchanged" $
        let con = bareECon "Just" arity1ConType
            arg = ivar "x"
            m = mkModule [DConstant "c" (EApp Type.bool con (ne [arg]))]
         in runPass constructorSaturation m `shouldBe` Right m

      it "arity-2 constructor applied to exactly 2 arguments: unchanged" $
        let con = bareECon "Pair" arity2ConType
            m = mkModule [DConstant "c" (EApp Type.bool con (ne [ivar "x", ivar "y"]))]
         in runPass constructorSaturation m `shouldBe` Right m

    describe "eta-expands partial constructor applications" $ do
      it "bare ECon with arity 1  =>  ELam [Just.0] (EApp Just [Just.0])" $
        let input = mkModule [DConstant "c" (bareECon "Just" arity1ConType)]
            freshParam = Label Type.int32 "Just.0"
            expected =
              mkModule
                [ DConstant
                    "c"
                    ( ELam
                        (ne [freshParam])
                        (EApp Type.bool (bareECon "Just" arity1ConType) (ne [EVar freshParam]))
                    )
                ]
         in runPass constructorSaturation input `shouldBe` Right expected

      it "bare ECon with arity 2  =>  ELam [P.0, P.1] (EApp P [P.0, P.1])" $
        let input = mkModule [DConstant "c" (bareECon "P" arity2ConType)]
            p0 = Label Type.int32 "P.0"
            p1 = Label Type.int32 "P.1"
            expected =
              mkModule
                [ DConstant
                    "c"
                    ( ELam
                        (ne [p0, p1])
                        (EApp Type.bool (bareECon "P" arity2ConType) (ne [EVar p0, EVar p1]))
                    )
                ]
         in runPass constructorSaturation input `shouldBe` Right expected

      it "arity-2 constructor applied to 1 argument  =>  ELam [P.0] (EApp P [arg, P.0])" $
        let con = bareECon "P" arity2ConType
            arg = ivar "x"
            input = mkModule [DConstant "c" (EApp (Type.int32 ~> Type.bool) con (ne [arg]))]
            p0 = Label Type.int32 "P.0"
            expected =
              mkModule
                [ DConstant
                    "c"
                    ( ELam
                        (ne [p0])
                        (EApp Type.bool con (ne [arg, EVar p0]))
                    )
                ]
         in runPass constructorSaturation input `shouldBe` Right expected

    describe "reports error for over-saturated constructors" $ do
      it "arity-1 constructor applied to 2 arguments  =>  Left (OverSaturatedConstructor)" $
        let con = bareECon "Just" arity1ConType
            input = mkModule [DConstant "c" (EApp TOpq con (ne [ivar "x", ivar "y"]))]
         in runPass constructorSaturation input
              `shouldBe` Left (OverSaturatedConstructor "Just")

    describe "invariant: checkConstructorsSaturated [] on every object body" $ do
      it "after saturating a bare arity-1 constructor" $
        let input = mkModule [DConstant "c" (bareECon "Just" arity1ConType)]
         in case runPass constructorSaturation input of
              Left err -> fail (show err)
              Right m ->
                concatMap
                  ( \case
                      DFunction _ _ _ body -> checkConstructorsSaturated body
                      DConstant _ body -> checkConstructorsSaturated body
                      _ -> []
                  )
                  (moduleObjects m)
                  `shouldBe` []
