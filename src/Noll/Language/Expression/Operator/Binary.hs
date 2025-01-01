{-# LANGUAGE StrictData #-}

module Noll.Language.Expression.Operator.Binary (BinaryOperator (..)) where

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
  | -- | Forward application (|.)
    OForwardApplication
  | -- | Reverse application (.|)
    OReverseApplication
  | -- | Flipped forward application ($.)
    OFlippedForwardApplication
  | -- | Flipped reverse application (.$)
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
