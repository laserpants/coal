{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Kernel.LLVM.IREval.Conceal (irConceal, irConcealArgs, irReveal) where

import Control.Monad (unless)
import Noll.Common.List1 (List1, fromList1)
import Noll.Kernel.LLVM.IREval (IREval (..))
import Noll.Kernel.LLVM.IREval.Comment (irComment)
import Noll.Kernel.LLVM.IREval.Malloc (irMalloc)
import Noll.Kernel.LLVM.IRInstruction (IRInstr)
import Noll.Kernel.LLVM.IRInstruction.TH
import Noll.Kernel.LLVM.IRType (IRType (..), IRTyped (..))
import Noll.Kernel.LLVM.IRType.Syntax (i1, i32, i64, i8, i8Ptr, ptr)
import Noll.Kernel.LLVM.IRValue (IRValue (..))
import Extra (forM)

irBox :: IRValue -> IRType -> IRInstr IRValue
irBox v t = do
  r1 <- irMalloc t
  store v r1
  bitcast r1 i8Ptr

irConceal :: IRValue -> IRInstr IRValue
irConceal v =
  case irTypeOf v of
    TInt1 ->
      inttoptr v i8Ptr
    TInt8 ->
      inttoptr v i8Ptr
    TInt32 ->
      inttoptr v i8Ptr
    TInt64 ->
      inttoptr v i8Ptr
    TFloat ->
      irBox v TFloat
    TDouble ->
      irBox v TDouble
    _ ->
      pure v

irUnbox :: IRValue -> IRType -> IRInstr IRValue
irUnbox v t = do
  p1 <- bitcast v (ptr t)
  load t p1

irConcealArgs :: (IREval e) => List1 e -> IRInstr [IRValue]
irConcealArgs args = do
  vs <- forM args $
    \e -> do
      v1 <- irEval e
      v2 <- irConceal v1
      unless (v1 == v2) (irComment ["^ Conceal arg."])
      pure v2
  pure (fromList1 vs)

irReveal :: IRValue -> IRType -> IRInstr IRValue
irReveal v =
  \case
    TInt1 ->
      ptrtoint v i1
    TInt8 ->
      ptrtoint v i8
    TInt32 ->
      ptrtoint v i32
    TInt64 ->
      ptrtoint v i64
    TFloat ->
      irUnbox v TFloat
    TDouble ->
      irUnbox v TDouble
    _ ->
      pure v
