{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

{- |
Module declarations.

A Coal kernel language module consists of:

  * A qualified name
  * A list of imported names
  * A list of top-level object declarations (functions, constants, data types,
    externals)

Modules are the compilation unit for kernel language programs. All modules in a
program are processed together, allowing cross-module references to be resolved
during type checking and compilation.
-}
module Coal.Kernel.Language.Module (Module (..)) where

import Coal.Kernel.Language.Object (Object (..))
import Common (Name)

data Module t = Module
  { moduleName :: Name
  , moduleImports :: [Name]
  , moduleObjects :: [Object t]
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Functor
    , Foldable
    , Traversable
    )
