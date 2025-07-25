{-# LANGUAGE OverloadedStrings #-}

module Noll.Kernel.LLVM.IREval.Closure.Extend (irExtend) where

import Noll.Common.Label (Label (..))
import Noll.Kernel.LLVM.IRConstruct (IRConstruct (..))
import Noll.Kernel.LLVM.IREval.Closure (closureStructType)
import Noll.Kernel.LLVM.IREval.Comment (irComment)
import Noll.Kernel.LLVM.IRInstruction (ICmpCond (..), IRInstr)
import Noll.Kernel.LLVM.IRInstruction.TH
import Noll.Kernel.LLVM.IRInterpreter
import Noll.Kernel.LLVM.IRInterpreter.Monad
import Noll.Kernel.LLVM.IRType (IRType (..))
import Noll.Kernel.LLVM.IRType.Syntax (i1, i32, i8Ptr, i8PtrPtr, ptr)
import Noll.Kernel.LLVM.IRValue (IRValue (..))

irClosureExtend :: IRValue -> IRValue -> IRValue -> IRInstr ()
irClosureExtend argF argN argAs = do
  let t = TNamed "closure" closureStructType
  r1 <- bitcast argF (ptr t)
  r2 <- getelementptr t r1 (I32 0) (I32 2)
  irComment ["Target function"]
  r3 <- load i8Ptr r2
  r4 <- getelementptr t r1 (I32 0) (I32 0)
  irComment ["# of captured arguments"]
  r5 <- load i32 r4
  r6 <- getelementptr t r1 (I32 0) (I32 1)
  irComment ["# of remaining arguments"]
  r7 <- load i32 r6
  irComment ["New # of captured arguments"]
  r8 <- add i32 r5 argN
  irComment ["New # of remaining arguments"]
  r9 <- sub i32 r7 argN
  --
  r10 <- sub i32 r8 (I32 1)
  r11 <- mul i32 r10 (I32 8)
  r12 <- gepsize t (I32 1)
  r13 <- ptrtoint r12 i32
  r14 <- add i32 r13 r11
  r15 <- callg i8Ptr "gc_malloc" [r14]
  r16 <- bitcast r15 (ptr t)
  r17 <- getelementptr t r16 (I32 0) (I32 0)
  store r8 r17
  r18 <- getelementptr t r16 (I32 0) (I32 1)
  store r9 r18
  r19 <- getelementptr t r16 (I32 0) (I32 2)
  store r3 r19
  r20 <- getelementptr t r16 (I32 0) (I32 3)
  r21 <- getelementptr t r1 (I32 0) (I32 3)
  irComment ["Loop counter"]
  r22 <- alloca1 i32
  store (I32 0) r22
  --
  labelFstLoop <- label "fst_loop"
  labelFstLoopBody <- label "fst_loop_body"
  labelSndLoop <- label "snd_loop"
  labelSndLoopBody <- label "snd_loop_body"
  labelSndLoopExit <- label "loop_exit"
  br1 labelFstLoop
  --
  (_, r) <- block labelFstLoop $ do
    r23 <- load i32 r22
    irComment ["Compare counter < captured # of args"]
    r24 <- icmp SLt i1 r23 r5
    br r24 [labelFstLoopBody, labelSndLoop]
    pure r23
  block1 labelFstLoopBody $ do
    r25 <- getelementptr1 i8Ptr r21 r
    r26 <- load i8Ptr r25
    r27 <- getelementptr1 i8Ptr r20 r
    store r26 r27
    irComment ["Increment counter"]
    r28 <- add i32 r (I32 1)
    store r28 r22
    irComment ["Jump back to loop condition"]
    br1 labelFstLoop
  (_, q) <- block labelSndLoop $ do
    r29 <- load i32 r22
    irComment ["Compare counter < new # of captured arguments"]
    r30 <- icmp SLt i1 r29 r8
    br r30 [labelSndLoopBody, labelSndLoopExit]
    pure r29
  block1 labelSndLoopBody $ do
    r31 <- sub i32 q r5
    r32 <- getelementptr1 i8Ptr argAs r31
    r33 <- load i8Ptr r32
    r34 <- getelementptr1 i8Ptr r20 q
    store r33 r34
    irComment ["Increment counter"]
    r35 <- add i32 q (I32 1)
    store r35 r22
    irComment ["Jump back to loop condition"]
    br1 labelSndLoop
  block1 labelSndLoopExit $ do
    ret r15

irExtend :: IRInterpreter (IRConstruct [IRLine])
irExtend =
  interpretFunction
    "closure_extend"
    (irClosureExtend arg0 arg1 arg2)
    [Label t name | Local t name <- [arg0, arg1, arg2]]
 where
  arg0 = Local i8Ptr "f"
  arg1 = Local i32 "n"
  arg2 = Local i8PtrPtr "as"
