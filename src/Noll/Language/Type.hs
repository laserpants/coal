{-# LANGUAGE StrictData #-}

module Noll.Language.Type (Type (..)) where

data Type o k
  = TArrow (Type o k) (Type o k)
  | TVariable (o k)
  deriving (Show, Eq, Ord, Read)
