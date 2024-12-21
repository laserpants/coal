{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Expression (Expression (..)) where

import Noll.Utils (Some)

data Expression t
  = -- | Function application
    Application t (Expression t) (Some (Expression t))
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
