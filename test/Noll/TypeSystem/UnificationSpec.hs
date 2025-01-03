{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystem.UnificationSpec where

import Control.Monad.Except (runExcept)
import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec (Spec, describe, it)
import Noll.TypeSystem.Substitution
import Noll.TypeSystem.Unification 
import Noll.Language (
  Type (..),
  Kind (..),
  Intrinsic (..),
  TypeIndex (..),
 )

spec :: Spec
spec =
  describe "Noll.TypeSystem.Unification" $ do
    describe "unify" $ do
      it "'0 ~ '0" $ do
        validateUnifyResult fixture1 fixture1 == Right mempty
      it "'0 ~ int32" $ do
        validateUnifyResult fixture1 (TIntrinsic IInt32) == Right (0 `mapsTo` TIntrinsic IInt32)
      it "int32 ~ '0" $ do
        validateUnifyResult (TIntrinsic IInt32) fixture1 == Right (0 `mapsTo` TIntrinsic IInt32)
      it "int32 ~ int32" $ do
        validateUnifyResult (TIntrinsic IInt32 :: Type TypeIndex Kind) (TIntrinsic IInt32) == Right mempty
      it "'0 ~ '1" $ do
        validateUnifyResult fixture1 fixture2 == Right (0 `mapsTo` fixture2)
      it "C('0) ~ C(int32)" $ do
        validateUnifyResult fixture3 fixture4 == Right (0 `mapsTo` TIntrinsic IInt32)
      it "int32 ~ bool" $ do
        validateUnifyResult (TIntrinsic IInt32 :: Type TypeIndex Kind) (TIntrinsic IBool) == Left CannotUnify
    describe "unifyAll" $ do
      it "'0 ~ int32 ~ bool" $ do
        validateUnifyAllResult
          [TVariable (TypeIndex KType 0), TIntrinsic IInt32, TIntrinsic IBool :: Type TypeIndex Kind]
          == Left CannotUnify
      it "'0 ~ bool ~ bool" $ do
        validateUnifyAllResult
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

validateUnifyResult :: (Unifiable a) => a -> a -> Either UnificationError Substitution
validateUnifyResult t1 t2 = runExcept (unify t1 t2)

validateUnifyAllResult :: (Unifiable a) => [a] -> Either UnificationError Substitution
validateUnifyAllResult ts = runExcept (unifyAll ts)

