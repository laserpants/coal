{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IRInstruction.Eval.Closure.Extend (irClosureExtend) where

import Control.Monad.State (evalStateT, get, modify)
import Noll.Core.LLVM.IRInstruction (IRInstr)
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRType (IRType (..))
import Noll.Core.LLVM.IRType.Syntax (fun, i1, i32, i64, i8Ptr, i8PtrPtr, ptr, struct)
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Utils (forM, forM_)
import TextShow (showt)

s applied = struct (i32 : replicate (3 + applied) i8Ptr)

xx n = TNamed ("closure" <> showt n) (s n)

irClosureExtend :: Int -> IRValue -> IRValue -> IRValue -> IRInstr ()
irClosureExtend n argF argN argAs = do
  r1 <- iBCast argF (ptr (xx n))
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
  rs <- forM [1 .. n] $
    \m -> do
      rm <- iGep (xx n) r1 (I32 0) (I32 (3 + fromIntegral m))
      iComment ""
      iComment ("Applied arg #" <> showt m)
      iComment ""
      iLoad i8Ptr rm
  flip evalStateT [] $
    forM_ [1 .. 3] $
      \m -> do
        rm <- iGep1 i8Ptr argAs (I32 (fromIntegral (m - 1)))
        iComment ""
        iComment ("Extra arg #" <> showt m)
        iComment ""
        r7 <- iLoad i8Ptr rm
        r8 <- iCmpSGt i1 argN (I32 (fromIntegral m))
        labelGt <- iLabel "gt"
        labelEq <- iLabel "eq"
        iBr r8 [labelGt, labelEq]
        modify (<> [r7])
        qs <- get
        let t = xx (n + m)
        iBlock_ labelEq $ do
          r9 <- iGepNull (ptr t) (I32 1)
          r10 <- iPtrtoint r9 i64
          r11 <- iCallGlobal i8Ptr "gc_malloc" [r10]
          r12 <- iBCast r11 (ptr t)
          r13 <- iGep t r12 (I32 0) (I32 0)
          iStore r4 r13
          r14 <- iGep t r12 (I32 0) (I32 1)
          r15 <- iBCast (Global (fun i8Ptr [i8Ptr, i32, i8PtrPtr]) ("closure" <> showt (n + m) <> "_finalize")) i8Ptr
          iStore r15 r14
          r16 <- iGep t r12 (I32 0) (I32 2)
          r17 <- iBCast (Global (fun i8Ptr [i8Ptr, i32, i8PtrPtr]) ("closure" <> showt (n + m) <> "_extend")) i8Ptr
          iStore r17 r16
          r18 <- iGep t r12 (I32 0) (I32 3)
          iStore r6 r18
          forM_ (zip (rs <> qs) [4 ..]) $ \(r, u) -> do
            r19 <- iGep t r12 (I32 0) (I32 u)
            iStore r r19
          iRet i8Ptr r11
          pure Null
        iBlock_ labelGt $ do
          pure Null
  -- pure r8
  iRet i8Ptr Null
