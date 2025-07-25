{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Expression.Operator (
  BinaryOperator (..),
  UnaryOperator (..),
  binaryOperatorTypeScheme,
) where

import Data.Data (Data, Typeable)
import Coal.Language.Type (IndexedType, Type (..), TypeIndex, (~>))
import Coal.Language.Type.Intrinsic (Intrinsic (..))
import Coal.Language.Type.Kind (Kind)
import Coal.Language.Type.Scheme (IndexedScheme, Scheme (..), forall0, forall1, forall2, forall3)

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
  | -- | Flipped forward application (.$)
    OFlippedForwardApplication
  | -- | Flipped reverse application ($.)
    OFlippedReverseApplication
  | -- | Forward composition (>>)
    OForwardComposition
  | -- | Reverse composition (<<)
    OReverseComposition
  | -- | String concatenation (...)
    OStringConcatenation
  | -- | List concatenation (++)
    OListConcatenation
  deriving (Show, Eq, Ord, Read, Data, Typeable)

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
      forall1 (\a -> TIntrinsic (IList a) ~> TIntrinsic (IList a) ~> TIntrinsic (IList a))
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
    o ->
      error ("TODO: " <> show o)
