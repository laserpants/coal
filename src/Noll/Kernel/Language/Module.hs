{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Kernel.Language.Module (Module (..)) where

import Noll.Kernel.Language.Object (Object)
import Extra (Name)

data Module t i e = Module
  { moduleName :: Name
  , moduleImports :: [i]
  , moduleObjects :: [Object t e]
  }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
