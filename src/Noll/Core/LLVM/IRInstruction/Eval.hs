{-# LANGUAGE LambdaCase #-}

module Noll.Core.LLVM.IRInstruction.Eval (irEvalExpr) where

import Control.Arrow ((>>>))
import Data.Functor.Foldable (project)
import Noll.Core.LLVM.IRInstruction (IRInstr)
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRType (IRType (..), IRTyped (..), i1, i32, i64, i8, i8Ptr)
import Noll.Core.LLVM.IRValue (IRValue (..), irPrimValue)

import qualified Noll.Core.Language as Core

irConceal :: IRValue -> IRInstr IRValue
irConceal v =
  case irTypeOf v of
    TInt1 ->
      iInttoptr v i8Ptr
    TInt8 ->
      iInttoptr v i8Ptr
    TInt32 ->
      iInttoptr v i8Ptr
    TInt64 ->
      iInttoptr v i8Ptr
    _ ->
      pure v

irReveal :: IRValue -> IRType -> IRInstr IRValue
irReveal v =
  \case
    TInt1 ->
      iPtrtoint v i1
    TInt8 ->
      iPtrtoint v i8
    TInt32 ->
      iPtrtoint v i32
    TInt64 ->
      iPtrtoint v i64
    _ ->
      pure v

irEvalOp op =
  undefined

irEvalExpr :: Core.Expr Core.Type -> IRInstr IRValue
irEvalExpr =
  project
    >>> \case
      Core.EOp op ->
        irEvalOp op
      Core.ELit Core.PChar{} ->
        error "TODO"
      Core.ELit Core.PString{} ->
        error "TODO"
      Core.ELit prim ->
        irConceal (irPrimValue prim)
