{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}

{- |
Function and closure compilation.

This module generates LLVM IR for top-level functions, including:

  * Standard function definitions with typed parameters
  * Trampolines for uniform closure application (@$apply@ functions)
  * Lazy-evaluation thunks for constant expressions

= Closure representation

Functions with arity > 0 are compiled to both a direct implementation and a
trampoline. The trampoline provides a uniform @ptr(ptr...) -> ptr@ interface
for the runtime's @rtApply@ mechanism, which handles partial application and
currying.
-}
module Coal.Kernel.LLVM.Function (
  irFunction,
  irTrampoline,
  irThunk,
) where

import Control.Monad (forM)
import Control.Monad.Reader (local)

import LLVM.IR
import qualified LLVM.IROperand.Constructors as O

import qualified Coal.Kernel.LLVM.Boxing as Boxing
import Coal.Kernel.LLVM.Monad (IRCodegen)
import Coal.Kernel.Language.Expr (Expr (..), Label (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Language.Type.HasType (HasType (typeOf))
import Common (Name)
import qualified Common.Environment as Environment

{- | Generate a function definition with the specified name, parameters, and
body.

The function signature uses the concrete IR types for each parameter, and the
body is evaluated as a tail expression.
-}
irFunction :: (Expr Type -> IRCodegen ()) -> Name -> [Label Type] -> Expr Type -> IRCodegen ()
irFunction irTail name lls expr =
  define rty name argts LExternal [] $
    local
      (Environment.insertMultiple [(n, OLocal t n) | (t, n) <- argts])
      (irTail expr)
 where
  rty = Boxing.irTypeRep (typeOf expr)
  argts = [(Boxing.irValueTypeRep t, n) | Label t n <- lls]

{- | Generate a uniform @ptr(ptr...) -> ptr@ trampoline for a function.

The closure machinery (@rtApply@) can call the trampoline without knowing the
function's concrete parameter types. The trampoline unboxes each @ptr@
argument, calls the real function, and boxes the result back to @ptr@.
-}
irTrampoline :: IRLinkage -> Name -> [Label Type] -> Type -> IRCodegen ()
irTrampoline linkage name lls retType =
  define TPtr trampolineName [(TPtr, "args")] linkage [] $ do
    unboxed <- forM (zip [0 ..] lls) $
      \(i, Label t _) -> do
        slot <- gep TPtr (OLocal TPtr "args") [O.i32 @Int i]
        boxed <- load TPtr slot
        Boxing.irUnbox t boxed
    result <- call NoTail (Boxing.irTypeRep retType) (OGlobal realFunType name) unboxed
    boxed <- Boxing.irBox retType result
    ret boxed
 where
  trampolineName = name <> "$apply"
  realFunType = TFun (Boxing.irTypeRep retType) [Boxing.irValueTypeRep t | Label t _ <- lls]

{- | Generate a lazy-evaluation thunk for a constant expression.

Creates three entities:

  * @cell#_\<name\>@: a global variable to cache the computed value
    (initialized to null)
  * @make#_\<name\>@: an internal function that evaluates the expression
  * @force#_\<name\>@: an external function that checks the cache, evaluates
    if needed, and stores the result
-}
irThunk :: (Expr Type -> IRCodegen ()) -> Name -> Expr Type -> IRCodegen ()
irThunk irTail name expr = do
  emitGlobal (IRVar LExternal cellName TPtr (CNull TPtr))
  define TPtr makeName [] LInternal [] $
    irTail expr
  define TPtr forceName [] LExternal [] $ mdo
    entryL <- block "entry"
    val <- load TPtr (OGlobal TPtr cellName)
    isNull <- icmp ICmpEq TPtr val (OConstant $ CNull TPtr)
    condbr isNull buildL doneL
    buildL <- block "build"
    built <- call NoTail TPtr (OGlobal (TFun TPtr []) makeName) []
    store built (OGlobal TPtr cellName)
    br doneL
    doneL <- block "done"
    result <- phi TPtr [(val, entryL), (built, buildL)]
    ret result
 where
  cellName = "cell#_" <> name
  makeName = "make#_" <> name
  forceName = "force#_" <> name
