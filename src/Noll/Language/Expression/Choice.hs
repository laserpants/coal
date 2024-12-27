{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Expression.Choice (Choice (..), Guard (..)) where

import Noll.Language.Pattern (Pattern)
import Noll.Utils (Some)

newtype Guard e a t = CGuard (e a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data Choice e a t
  = CPlain [Guard e a t] (e a t)
  | CLambda (Some (Pattern a t)) [Guard e a t] (e a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
