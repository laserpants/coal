{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Trait (Trait (..), With (..)) where

import Data.Data (Data, Typeable)
import Lang.Utils (Name)

-- | Standalone type trait
data Trait t = Trait Name t
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)

data With t = With [Trait t] t
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)
