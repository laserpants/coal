{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystem.UnificationSpec where

import Control.Monad.Except (runExcept)
import Data.List.NonEmpty (NonEmpty (..))
import Noll.Language (
  IndexedType,
  Intrinsic (..),
  Kind (..),
  Row (..),
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
        testUnifyTypes
          fixture1
          fixture1
          == Right mempty
      it "'0 ~ int32" $ do
        testUnifyTypes
          fixture1
          (TIntrinsic IInt32)
          == Right (0 `mapsTo` TIntrinsic IInt32)
      it "int32 ~ '0" $ do
        testUnifyTypes
          (TIntrinsic IInt32)
          fixture1
          == Right (0 `mapsTo` TIntrinsic IInt32)
      it "int32 ~ int32" $ do
        testUnifyTypes
          (TIntrinsic IInt32 :: Type TypeIndex Kind)
          (TIntrinsic IInt32)
          == Right mempty
      it "'0 ~ '1" $ do
        testUnifyTypes
          fixture1
          fixture2
          == Right (0 `mapsTo` fixture2)
      it "C('0) ~ C(int32)" $ do
        testUnifyTypes
          fixture3
          fixture4
          == Right (0 `mapsTo` TIntrinsic IInt32)
      it "int32 ~ bool" $ do
        testUnifyTypes
          (TIntrinsic IInt32 :: Type TypeIndex Kind)
          (TIntrinsic IBool)
          == Left CannotUnify
    describe "unifyAll" $ do
      it "'0 ~ int32 ~ bool" $ do
        testUnifyAllTypes
          [TVariable (TypeIndex KType 0), TIntrinsic IInt32, TIntrinsic IBool :: Type TypeIndex Kind]
          == Left CannotUnify
      it "'0 ~ bool ~ bool" $ do
        testUnifyAllTypes
          [TVariable (TypeIndex KType 0), TIntrinsic IBool, TIntrinsic IBool :: Type TypeIndex Kind]
          == Right (0 `mapsTo` TIntrinsic IBool)
    describe "unify rows" $ do
      it "{} ~ {}" $ do
        testUnifyRows
          fixture5
          fixture5
          == Right mempty
      it "{field : '0 | {}} ~ {field : '0 | {}}" $ do
        testUnifyRows
          fixture6
          fixture6
          == Right mempty
      it "{field : '0 | {}} ~ {}" $ do
        testUnifyRows
          fixture6
          fixture5
          == Left CannotUnify
      it "{one : '0 | two : '1 | {}} ~ {two : '1 | one : '0 | {}}" $ do
        testUnifyRows
          fixture7
          fixture8
          == Right mempty
      it "{one : '0 | two : '1 | {}} ~ {one : '0 | '2}" $ do
        testUnifyRows
          fixture7
          fixture9
          == Right (2 `mapsTo` TRow (RExtend "two" (TVariable (TypeIndex KType 1)) RNil))

fixture1 :: Type TypeIndex Kind
fixture1 = TVariable (TypeIndex KType 0)

fixture2 :: Type TypeIndex Kind
fixture2 = TVariable (TypeIndex KType 1)

fixture3 :: Type TypeIndex Kind
fixture3 = TApplication KType (TConstructor (KArrow KType KType) "C") (TVariable (TypeIndex KType 0) :| [])

fixture4 :: Type TypeIndex Kind
fixture4 = TApplication KType (TConstructor (KArrow KType KType) "C") (TIntrinsic IInt32 :| [])

fixture5 :: Row TypeIndex Kind IndexedType
fixture5 = RNil

fixture6 :: Row TypeIndex Kind IndexedType
fixture6 = RExtend "field" (TVariable (TypeIndex KType 0)) RNil

fixture7 :: Row TypeIndex Kind IndexedType
fixture7 = RExtend "one" (TVariable (TypeIndex KType 0)) (RExtend "two" (TVariable (TypeIndex KType 1)) RNil)

fixture8 :: Row TypeIndex Kind IndexedType
fixture8 = RExtend "two" (TVariable (TypeIndex KType 1)) (RExtend "one" (TVariable (TypeIndex KType 0)) RNil)

fixture9 :: Row TypeIndex Kind IndexedType
fixture9 = RExtend "one" (TVariable (TypeIndex KType 0)) (RVariable (TypeIndex KRow 2))

testUnifyTypes :: Type TypeIndex Kind -> Type TypeIndex Kind -> Either UnificationError Substitution
testUnifyTypes t1 t2 = runUnifier (freshIdIn [t1, t2]) (unify t1 t2)

testUnifyAllTypes :: [Type TypeIndex Kind] -> Either UnificationError Substitution
testUnifyAllTypes ts = runUnifier (freshIdIn ts) (unifyAll ts)

testUnifyRows :: Row TypeIndex Kind (Type TypeIndex Kind) -> Row TypeIndex Kind (Type TypeIndex Kind) -> Either UnificationError Substitution
testUnifyRows r1 r2 = runUnifier (freshIdIn [r1, r2]) (unify r1 r2)
