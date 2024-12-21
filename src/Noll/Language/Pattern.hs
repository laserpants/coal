{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Pattern (Pattern (..)) where

data Pattern t
  = PAny t
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
