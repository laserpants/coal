{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Type.Row (Row (..)) where

import Noll.Utils (Name)

data Row o k t
  = Extend Name t (Row o k t)
  | Variable (o k)
  | Nil
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
