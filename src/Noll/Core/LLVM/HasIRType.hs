{-# LANGUAGE LambdaCase #-}

module Noll.Core.LLVM.HasIRType (HasIRType (..)) where

import Noll.Core.LLVM.IRType (IRType (..))
import Noll.Core.LLVM.IRValue (IRValue)
import qualified Noll.Core.LLVM.IRValue as IR

class HasIRType t where
  irTypeOf :: t -> IRType

instance HasIRType IRType where
  irTypeOf = id

instance HasIRType IRValue where
  irTypeOf =
    \case
      IR.Local t _ ->
        t
      IR.Global t _ ->
        t
      IR.I1{} ->
        TInt1
      IR.I32{} ->
        TInt32
      IR.I64{} ->
        TInt64
      IR.Float{} ->
        TFloat
      IR.Double{} ->
        TDouble
      IR.Null ->
        TPtr TInt8
