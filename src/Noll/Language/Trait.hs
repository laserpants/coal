{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Trait (Trait (..), Uses (..)) where

import Data.Data (Data, Typeable)
import Noll.Utils (Name)

-- | Standalone type trait
data Trait t = Trait Name t
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)

data Uses t = Uses [Trait t] t
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)
