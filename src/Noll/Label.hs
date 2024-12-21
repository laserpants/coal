{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Label (Label (..)) where

import Noll.Utils (Name)

data Label t = Label t Name
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
