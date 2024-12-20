{-# LANGUAGE StrictData #-}

module Noll.Language.Type.Kind (Kind (..)) where

data Kind o
  = Type
  | Row
  | Arrow (Kind o) (Kind o)
  | Trait
  | Variable o
  deriving (Show, Eq, Ord, Read)

infixr 1 `Arrow`
