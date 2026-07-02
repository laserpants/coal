{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.LegacyKernel.Language.Module (Module (..)) where

import Coal.LegacyKernel.Language.Object (Object)
import Extras (Name)

data Module t i e = Module
  { moduleName :: Name
  , moduleImports :: [i]
  , moduleObjects :: [Object t e]
  }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
