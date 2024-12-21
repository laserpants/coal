{-# LANGUAGE LambdaCase #-}

module Noll.Core.LLVM.IRType.IRTypeOf (IRTypeOf (..)) where

import Noll.Core.LLVM.IRType (IRType (..))
import Noll.Core.LLVM.IRValue (IRValue)
import qualified Noll.Core.LLVM.IRValue as IR

class IRTypeOf t where
  irTypeOf :: t -> IRType

instance IRTypeOf IRType where
  irTypeOf = id

instance IRTypeOf IRValue where
  irTypeOf =
    \case
      IR.Local t _ ->
        t
      IR.Global t _ ->
        t
      IR.I1{} ->
        Int1
      IR.I32{} ->
        Int32
      IR.I64{} ->
        Int64
      IR.Float{} ->
        Float
      IR.Double{} ->
        Double
      IR.Null ->
        Ptr Int8
