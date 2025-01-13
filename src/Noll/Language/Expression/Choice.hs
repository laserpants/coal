{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Expression.Choice (Choice (..), Guard (..)) where

import Noll.Common.List1 (List1)
import Noll.Language.Pattern (Pattern)

newtype Guard e a t = CGuard (e a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data Choice e a t
  = CPlain a [Guard e a t] (e a t)
  | CLambda a (List1 (Pattern a t)) [Guard e a t] (e a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
