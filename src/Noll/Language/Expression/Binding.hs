{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Expression.Binding (Binding (..)) where

import Noll.Language.Pattern (Pattern (..))
import Noll.Utils (Name, Some)

data Binding e a t
  = BPattern (Pattern a t) (e a t)
  | BFunction Name (Some (Pattern a t)) (e a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
