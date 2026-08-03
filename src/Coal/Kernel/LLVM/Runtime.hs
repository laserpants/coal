{-# LANGUAGE NamedFieldPuns #-}

{- |
Runtime function invocation helpers.

Provides high-level wrappers for calling external runtime functions and
user-defined @external@ declarations. Handles function declaration emission
and call-site construction.
-}
module Coal.Kernel.LLVM.Runtime (
  callRuntime,
  callRuntimeTail,
  irCallExternal,
) where

import LLVM.IR

import Coal.Common.Name (Name)
import Coal.Kernel.LLVM.Monad (IRCodegen)
import Coal.Kernel.LLVM.RuntimeDefs (RuntimeFun (..))

irCallExternal :: Name -> IRType -> [IROperand] -> IRCodegen IROperand
irCallExternal name (TFun rty ts) args = do
  declare name rty ts
  call NoTail rty (OGlobal (TFun rty ts) name) args
irCallExternal _ _ _ =
  error "Internal error"

-- | Call a runtime function in non-tail position (default).
callRuntime :: RuntimeFun -> [IROperand] -> IRCodegen IROperand
callRuntime RuntimeFun{rtName, rtRetType, rtArgTypes} =
  irCallExternal rtName (TFun rtRetType rtArgTypes)

{- | Call a runtime function as a true LLVM tail call.

The caller must ensure the call is in tail position and that no stack-allocated
memory is passed that the callee might read after the caller frame is gone.
Use heap allocation (e.g. @rt_alloc@) for argument vectors.
-}
callRuntimeTail :: RuntimeFun -> [IROperand] -> IRCodegen IROperand
callRuntimeTail RuntimeFun{rtName, rtRetType, rtArgTypes} args = do
  declare rtName rtRetType rtArgTypes
  call Tail rtRetType (OGlobal (TFun rtRetType rtArgTypes) rtName) args
