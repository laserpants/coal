{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystem.UnificationSpec where

import Control.Monad.Except (runExcept)
import Data.List.NonEmpty (NonEmpty (..))
import Noll.Language (
  Intrinsic (..),
  Kind (..),
  Type (..),
  TypeIndex (..),
  freshIdIn,
 )
import Noll.TypeSystem.Substitution
import Noll.TypeSystem.Unification
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem.Unification" $ do
    describe "unify" $ do
      it "'0 ~ '0" $ do
        testUnifyTypes fixture1 fixture1 == Right mempty
      it "'0 ~ int32" $ do
        testUnifyTypes fixture1 (TIntrinsic IInt32) == Right (0 `mapsTo` TIntrinsic IInt32)
      it "int32 ~ '0" $ do
        testUnifyTypes (TIntrinsic IInt32) fixture1 == Right (0 `mapsTo` TIntrinsic IInt32)
      it "int32 ~ int32" $ do
        testUnifyTypes (TIntrinsic IInt32 :: Type TypeIndex Kind) (TIntrinsic IInt32) == Right mempty
      it "'0 ~ '1" $ do
        testUnifyTypes fixture1 fixture2 == Right (0 `mapsTo` fixture2)
      it "C('0) ~ C(int32)" $ do
        testUnifyTypes fixture3 fixture4 == Right (0 `mapsTo` TIntrinsic IInt32)
      it "int32 ~ bool" $ do
        testUnifyTypes (TIntrinsic IInt32 :: Type TypeIndex Kind) (TIntrinsic IBool) == Left CannotUnify
    describe "unifyAll" $ do
      it "'0 ~ int32 ~ bool" $ do
        testUnifyAllTypes
          [TVariable (TypeIndex KType 0), TIntrinsic IInt32, TIntrinsic IBool :: Type TypeIndex Kind]
          == Left CannotUnify
      it "'0 ~ bool ~ bool" $ do
        testUnifyAllTypes
          [TVariable (TypeIndex KType 0), TIntrinsic IBool, TIntrinsic IBool :: Type TypeIndex Kind]
          == Right (0 `mapsTo` TIntrinsic IBool)

fixture1 :: Type TypeIndex Kind
fixture1 = TVariable (TypeIndex KType 0)

fixture2 :: Type TypeIndex Kind
fixture2 = TVariable (TypeIndex KType 1)

fixture3 :: Type TypeIndex Kind
fixture3 = TApplication KType (TConstructor (KArrow KType KType) "C") (TVariable (TypeIndex KType 0) :| [])

fixture4 :: Type TypeIndex Kind
fixture4 = TApplication KType (TConstructor (KArrow KType KType) "C") (TIntrinsic IInt32 :| [])

testUnifyTypes :: Type TypeIndex Kind -> Type TypeIndex Kind -> Either UnificationError Substitution
testUnifyTypes t1 t2 = runUnifier (freshIdIn [t1, t2]) (unify t1 t2)

testUnifyAllTypes :: [Type TypeIndex Kind] -> Either UnificationError Substitution
testUnifyAllTypes ts = runUnifier (freshIdIn ts) (unifyAll ts)
