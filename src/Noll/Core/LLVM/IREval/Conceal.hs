{-# LANGUAGE LambdaCase #-}

module Noll.Core.LLVM.IREval.Conceal (
  irConceal,
  irReveal,
  irRevealExpr,
) where

import Noll.Core.LLVM.IREval (IREval (..))
import Noll.Core.LLVM.IRInstruction (IRInstr)
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRType (IRType (..), IRTyped (..))
import Noll.Core.LLVM.IRType.Syntax (i1, i32, i64, i8, i8Ptr)
import Noll.Core.LLVM.IRValue (IRValue (..))

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

irRevealExpr :: (IREval e, IRTyped e) => e -> IRInstr IRValue
irRevealExpr expr = do
  v <- irEval expr
  irReveal v (irTypeOf expr)
