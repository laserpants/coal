{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Trait (Trait (..)) where

import Noll.Utils (Name)

-- | Standalone type trait
data Trait t = Trait Name t
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
