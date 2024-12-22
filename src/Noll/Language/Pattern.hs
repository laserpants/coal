{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Pattern (Pattern (..)) where

import Noll.Label (Label (..))

data Pattern t
  = -- | Wildcard pattern
    Any t
  | -- | Variable pattern
    Variable (Label t)
  | -- | Data constructor pattern
    Constructor (Label t) [Pattern t]
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
