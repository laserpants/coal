{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Expression.Operator.Binary (BinaryOperator (..), binaryOperatorTypeScheme) where

import Noll.Language.Type (IndexedType, Type (..), TypeIndex, (~>))
import Noll.Language.Type.Intrinsic (Intrinsic (..))
import Noll.Language.Type.Kind (Kind)
import Noll.Language.Type.Scheme (Scheme (..), forall0, forall1, forall2, forall3)

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
  deriving (Show, Eq, Ord, Read)

binaryOperatorTypeScheme :: BinaryOperator -> Scheme TypeIndex Kind IndexedType
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
    OListConcatenation ->
      forall1 (\a -> TIntrinsic (IList a) ~> TIntrinsic (IList a) ~> TIntrinsic (IList a))
    OAddition ->
      forall1 (\a -> a ~> a ~> a)
