{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Expression.Operator (
  BinaryOperator (..),
  UnaryOperator (..),
  unaryOperatorTypeScheme,
  binaryOperatorTypeScheme,
) where

import Coal.Language.Type (Type (..), listType, (~>))
import Coal.Language.Type.Intrinsic (Intrinsic (..))
import Coal.Language.Type.Scheme (IndexedScheme, Scheme (..), forall0, forall1, forall2, forall3)
import Data.Data (Data, Typeable)

-- | Unary operators
data UnaryOperator
  = -- | Logical NOT (!)
    OLogicalNot
  | -- | Negation (-)
    ONegate
  deriving (Show, Eq, Ord, Read, Data, Typeable)

-- | Binary operators
data BinaryOperator
  = -- | Equality (==)
    OEqualTo
  | -- | Inequality (!=)
    ONotEqualTo
  | -- | Less than (<)
    OLessThan
  | -- | Greater than (>)
    OGreaterThan
  | -- | Less than or equal (<=)
    OLessThanOrEqual
  | -- | Greater than or equal (>=)
    OGreaterThanOrEqual
  | -- | Addition (+)
    OAddition
  | -- | Subtraction (-)
    OSubtraction
  | -- | Multiplication (*)
    OMultiplication
  | -- | Exponentiation (^)
    OExponentiation
  | -- | Modulus (%)
    OModulus
  | -- | Division (/)
    ODivision
  | -- | Logical OR (||)
    OLogicalOr
  | -- | Logical AND (&&)
    OLogicalAnd
  | -- | Forward application (.|)
    OForwardApplication
  | -- | Reverse application (|.)
    OReverseApplication
  | -- | Forward composition (>>)
    OForwardComposition
  | -- | Reverse composition (<<)
    OReverseComposition
  | -- | String concatenation (...)
    OStringConcatenation
  | -- | List concatenation (++)
    OListConcatenation
  | -- | Semmigroup operation (<>)
    OSemigroupOp
  deriving (Show, Eq, Ord, Read, Data, Typeable)

unaryOperatorTypeScheme :: UnaryOperator -> IndexedScheme
unaryOperatorTypeScheme =
  \case
    OLogicalNot ->
      forall0 (TIntrinsic IBool ~> TIntrinsic IBool)
    ONegate ->
      forall1 (\a -> a ~> a)

binaryOperatorTypeScheme :: BinaryOperator -> IndexedScheme
binaryOperatorTypeScheme =
  \case
    OReverseApplication ->
      forall2 (\a b -> a ~> (a ~> b) ~> b)
    OForwardApplication ->
      forall2 (\a b -> (a ~> b) ~> a ~> b)
    OReverseComposition ->
      forall3 (\a b c -> (b ~> c) ~> (a ~> b) ~> a ~> c)
    OForwardComposition ->
      forall3 (\a b c -> (a ~> b) ~> (b ~> c) ~> a ~> c)
    OLogicalOr ->
      forall0 (TIntrinsic IBool ~> TIntrinsic IBool ~> TIntrinsic IBool)
    OLogicalAnd ->
      forall0 (TIntrinsic IBool ~> TIntrinsic IBool ~> TIntrinsic IBool)
    OEqualTo ->
      forall1 (\a -> a ~> a ~> TIntrinsic IBool)
    ONotEqualTo ->
      forall1 (\a -> a ~> a ~> TIntrinsic IBool)
    OListConcatenation ->
      forall1 (\a -> listType a ~> listType a ~> listType a)
    OStringConcatenation ->
      Forall mempty [] (TIntrinsic IString ~> TIntrinsic IString ~> TIntrinsic IString)
    OAddition ->
      forall1 (\a -> a ~> a ~> a)
    OSubtraction ->
      forall1 (\a -> a ~> a ~> a)
    OMultiplication ->
      forall1 (\a -> a ~> a ~> a)
    OLessThan ->
      forall1 (\a -> a ~> a ~> TIntrinsic IBool)
    OGreaterThan ->
      forall1 (\a -> a ~> a ~> TIntrinsic IBool)
    OLessThanOrEqual ->
      forall1 (\a -> a ~> a ~> TIntrinsic IBool)
    OGreaterThanOrEqual ->
      forall1 (\a -> a ~> a ~> TIntrinsic IBool)
    OExponentiation{} ->
      forall1 (\a -> a ~> a ~> a)
    OModulus{} ->
      forall1 (\a -> a ~> a ~> a)
    ODivision{} ->
      forall1 (\a -> a ~> a ~> a)
    OSemigroupOp{} ->
      forall1 (\a -> a ~> a ~> a)
