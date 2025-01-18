{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Module.Constant (Constant (..)) where

import Noll.Language.Trait (Uses (..))

data Constant e a t = Constant a (Uses t) (e a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
