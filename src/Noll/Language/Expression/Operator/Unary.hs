{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Expression.Operator.Unary (UnaryOperator (..)) where

import Data.Data (Data, Typeable)

-- | Unary operators
data UnaryOperator
  = -- | Logical NOT (!)
    OLogicalNot
  | -- | Negation (-)
    ONegate
  deriving (Show, Eq, Ord, Read, Data, Typeable)
