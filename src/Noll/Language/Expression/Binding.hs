{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Expression.Binding (Binding (..)) where

import Noll.Language.Pattern (Pattern (..))
import Noll.Utils (Name, Some)

data Binding e t
  = Pattern (Pattern t) (e t)
  | Function Name (Some (Pattern t)) (e t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
