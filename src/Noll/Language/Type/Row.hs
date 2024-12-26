{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Type.Row (Row (..)) where

import Noll.Utils (Dictionary, Name)

data Row o k t
  = RExtend Name t (Row o k t)
  | RVariable (o k)
  | RNil
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data RowMap o k t = RowMap (Dictionary [t]) (Row o k t)
  deriving (Show, Eq, Ord, Read)
