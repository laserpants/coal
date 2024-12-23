{-# LANGUAGE StrictData #-}

module Noll.Language.Expression.Operator.Binary (BinaryOperator (..)) where

-- | Binary operators
data BinaryOperator
  = -- | Equality (==)
    EqualTo
  | -- | Inequality (!=)
    NotEqualTo
  | -- | Less than (<)
    LessThan
  | -- | Greater than (>)
    GreaterThan
  | -- | Less than or equal (<=)
    LessThanOrEqual
  | -- | Greater than or equal (>=)
    GreaterThanOrEqual
  | -- | Addition (+)
    Addition
  | -- | Subtraction (-)
    Subtraction
  | -- | Multiplication (*)
    Multiplication
  | -- | Exponentiation (^)
    Exponentiation
  | -- | Division (/)
    Division
  | -- | Logical OR (||)
    LogicalOr
  | -- | Logical AND (&&)
    LogicalAnd
  | -- | Forward application (|.)
    ForwardApplication
  | -- | Reverse application (.|)
    ReverseApplication
  | -- | Flipped forward application ($.)
    FlippedForwardApplication
  | -- | Flipped reverse application (.$)
    FlippedReverseApplication
  | -- | Forward composition (>>)
    ForwardComposition
  | -- | Reverse composition (<<)
    ReverseComposition
  | -- | String concatenation (+++)
    StringConcatenation
  | -- | List concatenation (++)
    ListConcatenation
  deriving (Show, Eq, Ord, Read)
