{-# LANGUAGE LambdaCase #-}

{- |
Object-level type checking.

Type checks individual top-level objects (functions, constants). For
functions, introduces parameter bindings into the local environment before
checking the body expression.
-}
module Coal.Kernel.TypeCheck.Object (
  checkObject,
) where

import Control.Monad (void)
import Control.Monad.Reader (local)

import Coal.Kernel.Language.Expr (Label (..))
import Coal.Kernel.Language.Object (Object (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.TypeCheck.Env (setContext, withLocals)
import Coal.Kernel.TypeCheck.Error (Context (..))
import Coal.Kernel.TypeCheck.Expr (Check, checkExpr)

checkObject :: Object Type -> Check ()
checkObject =
  \case
    DFunction name params body ->
      let bindings = [(n, t) | Label t n <- params]
       in local (setContext (InObject name) . withLocals bindings) $
            void (checkExpr body)
    DConstant name expr ->
      local (setContext (InObject name)) $
        void (checkExpr expr)
    _ ->
      pure ()
