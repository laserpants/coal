{-# LANGUAGE StrictData #-}

module Noll.Language.Expression.Operator.Unary (UnaryOperator (..)) where

-- | Unary operators
data UnaryOperator
  = -- | Logical NOT (!)
    LogicalNot
  | -- | Negation (-)
    Negate
  deriving (Show, Eq, Ord, Read)
