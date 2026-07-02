{- |
Module-level type checking.

Provides the entry point for checking an entire module. Iterates over all
objects in the module, setting the appropriate context for error reporting.
-}
module Coal.Kernel.TypeCheck.Module (
  checkModule,
) where

import Control.Monad (forM_)
import Control.Monad.Reader (local)

import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.TypeCheck.Env (setContext)
import Coal.Kernel.TypeCheck.Error (Context (..))
import Coal.Kernel.TypeCheck.Expr (Check)
import Coal.Kernel.TypeCheck.Object (checkObject)

checkModule :: Module Type -> Check ()
checkModule (Module name _imports objects) =
  local (setContext (InModule name)) $
    forM_ objects checkObject
