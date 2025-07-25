{-# LANGUAGE OverloadedStrings #-}

module Coal.TypeSystem.UnificationSpec where

import Control.Monad.Except (runExcept)
import Data.List.NonEmpty (NonEmpty (..))
import Coal.Language (
  IndexedType,
  Intrinsic (..),
  Kind (..),
  Row (..),
  Type (..),
  TypeIndex (..),
  freshIdIn,
 )
import Coal.TypeSystem.Substitution
import Coal.TypeSystem.Unification
import Test.Hspec (Spec, describe, it)

import qualified Data.Map.Strict as Map
import qualified Coal.TypeSystem.Substitution as Substitution

spec :: Spec
spec =
  describe "Coal.TypeSystem.Unification" $ do
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
          == Left ECannotUnify
    describe "unifyAll" $ do
      it "'0 ~ int32 ~ bool" $ do
        testUnifyAllTypes
          [TVariable (TypeIndex KType 0), TIntrinsic IInt32, TIntrinsic IBool :: Type TypeIndex Kind]
          == Left ECannotUnify
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
          == Left ECannotUnify
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
      it "{one : int32 | two : '1 | {}} ~ {one : '0 | '2}" $ do
        testUnifyRows
          fixture10
          fixture9
          == Right
            ( Substitution.fromList
                [ (0, TIntrinsic IInt32)
                , (2, TRow (RExtend "two" (TVariable (TypeIndex KType 1)) RNil))
                ]
            )
      it "{one : int32 | two : '1 | {}} ~ {one : int32 | '2}" $ do
        testUnifyRows
          fixture10
          fixture11
          == Right
            ( Substitution.fromList
                [ (2, TRow (RExtend "two" (TVariable (TypeIndex KType 1)) RNil))
                ]
            )
      it "{two : '1 | one : int32 | {}} ~ {one : int32 | '2}" $ do
        testUnifyRows fixture12 fixture11
          `shouldInclude` (2, TRow (RExtend "two" (TVariable (TypeIndex KType 1)) RNil))

shouldInclude :: Either UnificationError Substitution -> (Int, Type TypeIndex Kind) -> Bool
shouldInclude result sub =
  case result of
    Left{} ->
      False
    Right subs ->
      sub `elem` Map.toList (substitutionMap subs)

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

fixture10 :: Row TypeIndex Kind IndexedType
fixture10 = RExtend "one" (TIntrinsic IInt32) (RExtend "two" (TVariable (TypeIndex KType 1)) RNil)

fixture11 :: Row TypeIndex Kind IndexedType
fixture11 = RExtend "one" (TIntrinsic IInt32) (RVariable (TypeIndex KRow 2))

fixture12 :: Row TypeIndex Kind IndexedType
fixture12 = RExtend "two" (TVariable (TypeIndex KType 1)) (RExtend "one" (TIntrinsic IInt32) RNil)

testUnifyTypes :: Type TypeIndex Kind -> Type TypeIndex Kind -> Either UnificationError Substitution
testUnifyTypes t1 t2 = evalUnifier (freshIdIn [t1, t2]) (unify t1 t2)

testUnifyAllTypes :: [Type TypeIndex Kind] -> Either UnificationError Substitution
testUnifyAllTypes ts = evalUnifier (freshIdIn ts) (unifyAll ts)

testUnifyRows :: Row TypeIndex Kind (Type TypeIndex Kind) -> Row TypeIndex Kind (Type TypeIndex Kind) -> Either UnificationError Substitution
testUnifyRows r1 r2 = evalUnifier (freshIdIn [r1, r2]) (unify r1 r2)
