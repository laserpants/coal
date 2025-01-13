{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Module.Global where

import Noll.Language.Trait (Uses (..))

data Global e a t = Global (Uses t) (e a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
