{-# LANGUAGE StrictData #-}

module Noll.Language.Expression.Operator.Unary (UnaryOperator (..)) where

-- | Unary operators
data UnaryOperator
  = -- | Logical NOT (!)
    OLogicalNot
  | -- | Negation (-)
    ONegate
  deriving (Show, Eq, Ord, Read)
