{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Pattern (Pattern (..)) where

import Noll.Label (Label (..))

data Pattern t
  = Any t
  | Variable (Label t)
  | Constructor (Label t) [Pattern t]
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
