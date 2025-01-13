{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Expression.Binding (Binding (..)) where

import Noll.Language.Pattern (Pattern (..))
import Noll.Common.List1 (List1)
import Noll.Utils (Name)

data Binding e a t
  = BPattern a (Pattern a t) (e a t)
  | BFunction a Name (List1 (Pattern a t)) (e a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
