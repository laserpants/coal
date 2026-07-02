{-# LANGUAGE NamedFieldPuns #-}

{- |
Runtime function invocation helpers.

Provides high-level wrappers for calling external runtime functions and
user-defined @external@ declarations. Handles function declaration emission
and call-site construction.
-}
module Coal.Kernel.LLVM.Runtime (
  callRuntime,
  irCallExternal,
) where

import LLVM.IR

import Coal.Kernel.LLVM.Monad (IRCodegen)
import Coal.Kernel.LLVM.RuntimeDefs (RuntimeFun (..))
import Common (Name)

irCallExternal :: Name -> IRType -> [IROperand] -> IRCodegen IROperand
irCallExternal name (TFun rty ts) args = do
  declare name rty ts
  call NoTail rty (OGlobal (TFun rty ts) name) args
irCallExternal _ _ _ =
  error "Internal error"

callRuntime :: RuntimeFun -> [IROperand] -> IRCodegen IROperand
callRuntime RuntimeFun{rtName, rtRetType, rtArgTypes} =
  irCallExternal rtName (TFun rtRetType rtArgTypes)
