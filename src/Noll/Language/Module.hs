{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Module (Module (..)) where

import Noll.Language.Module.Object (Object (..), Path (..))
import Noll.Utils (Name)

data Module a k t = Module Path [Name] [Object a k t]
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
