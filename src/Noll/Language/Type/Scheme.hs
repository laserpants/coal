{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Type.Scheme (Scheme (..)) where

import Data.Set (Set)
import Noll.Language.Trait (Trait (..))

data Scheme o k t = Forall (Set (o k)) [Trait t] t
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
