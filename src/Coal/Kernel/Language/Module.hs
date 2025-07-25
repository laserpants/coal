{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Kernel.Language.Module (Module (..)) where

import Extra (Name)
import Coal.Kernel.Language.Object (Object)

data Module t i e = Module
  { moduleName :: Name
  , moduleImports :: [i]
  , moduleObjects :: [Object t e]
  }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
