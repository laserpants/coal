{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Module.Function where

import Noll.Language.Pattern (Pattern)
import Noll.Language.Trait (Uses (..))
import Noll.Lib.List1 (List1)

data Function e a t = Function (Uses t) (List1 (Pattern a t)) (e a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
