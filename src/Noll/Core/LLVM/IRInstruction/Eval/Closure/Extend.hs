{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IRInstruction.Eval.Closure.Extend (irClosureExtend) where

import Debug.Trace
import Noll.Core.LLVM.IRInstruction (IRInstr)
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRType (IRType (..))
import Noll.Core.LLVM.IRType.Syntax (i32, i8Ptr, ptr, struct)
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Utils (forM_)
import TextShow (showt)

s applied = struct (i32 : replicate (3 + applied) i8Ptr)

xx n = TNamed ("closure" <> showt n) (s n)

irClosureExtend :: Int -> IRValue -> IRValue -> IRValue -> IRInstr IRValue
irClosureExtend n argF argN argAs = do
  r1 <- iBCast (Local i8Ptr "f") (ptr (xx n))
  r2 <- iGep (xx n) r1 (I32 0) (I32 0)
  iComment ""
  iComment "Argument count"
  iComment ""
  r3 <- iLoad i32 r2
  iComment ""
  iComment "New argument count"
  iComment ""
  r4 <- iSub i32 r3 argN
  r5 <- iGep (xx n) r1 (I32 0) (I32 3)
  iComment ""
  iComment "Target function"
  iComment ""
  r6 <- iLoad i8Ptr r5
  forM_ [1 .. n] $ \m -> do
    rm <- iGep (xx n) r1 (I32 0) (I32 (3 + fromIntegral m))
    iComment ""
    iComment ("Applied arg #" <> showt m)
    iComment ""
    iLoad i8Ptr rm
  iComment ""
  iComment "Extra arg #1"
  iComment ""
  pure r2
