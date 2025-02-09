{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Module.Function (Function (..)) where

import Data.Data (Data, Typeable)
import Noll.Common.List1 (List1)
import Noll.Language.Pattern (Pattern)
import Noll.Language.Trait (Uses (..))

data Function e a t = Function a (Uses t) (List1 (Pattern a t)) (e a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)
