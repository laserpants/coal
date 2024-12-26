{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Type.Row (Row (..)) where

import Noll.Utils (Name)

data Row o k t
  = RExtend Name t (Row o k t)
  | RVariable (o k)
  | RNil
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
