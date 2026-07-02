{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Closure finalization for saturated function applications

When a closure has all required arguments, this function:
1. Assembles complete argument array (captured + supplied)
2. Calls the target function via @call_n@
3. Returns the result

= Stack Allocation

Uses stack allocation (@alloca@) for the assembled argument array
since it's temporary and known to be used only during the call.
-}
module Coal.LegacyKernel.LLVM.IREval.Closure.Finalize (irFinalize) where

import Coal.Common.Label (Label (..))
import Coal.LegacyKernel.LLVM.IRConstruct (IRConstruct (..))
import Coal.LegacyKernel.LLVM.IREval.Closure (closureStructType)
import Coal.LegacyKernel.LLVM.IREval.Comment (irComment)
import Coal.LegacyKernel.LLVM.IRInstruction (ICmpCond (..), IRInstr)
import Coal.LegacyKernel.LLVM.IRInstruction.Builders
import Coal.LegacyKernel.LLVM.IRInterpreter (interpretFunction)
import Coal.LegacyKernel.LLVM.IRInterpreter.Monad (IRInterpreter, IRLine)
import Coal.LegacyKernel.LLVM.IRType (IRType (..))
import Coal.LegacyKernel.LLVM.IRType.Syntax (i1, i32, i8Ptr, i8PtrPtr, ptr)
import Coal.LegacyKernel.LLVM.IRValue (IRValue (..))

irClosureFinalize :: IRValue -> IRValue -> IRInstr ()
irClosureFinalize argF argAs = do
  let t = TNamed "closure" (closureStructType 0)
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
  r8 <- add i32 r5 r7
  irComment ["Arg array"]
  r9 <- alloca i8Ptr r8
  r10 <- getelementptr t r1 (I32 0) (I32 3)
  irComment ["Loop counter"]
  r11 <- alloca1 i32
  store (I32 0) r11
  labelFstLoop <- label "fst_loop"
  labelFstLoopBody <- label "fst_loop_body"
  labelSndLoop <- label "snd_loop"
  labelSndLoopBody <- label "snd_loop_body"
  labelSndLoopExit <- label "loop_exit"
  br1 labelFstLoop
  -- First loop: copy captured args (indices 0..r5-1)
  (_, r) <- block labelFstLoop $ do
    r12 <- load i32 r11
    irComment ["Compare counter < captured # of args"]
    r13 <- icmp SLt i1 r12 r5
    br r13 [labelFstLoopBody, labelSndLoop]
    pure r12
  block1 labelFstLoopBody $ do
    -- Copy closure.args[r] to assembled_array[r]
    r14 <- getelementptr1 i8Ptr r10 r
    r15 <- load i8Ptr r14
    r16 <- getelementptr1 i8Ptr r9 r
    store r15 r16
    irComment ["Increment counter"]
    r17 <- add i32 r (I32 1)
    store r17 r11
    irComment ["Jump back to loop condition"]
    br1 labelFstLoop
  -- Second loop: copy supplied args (indices r5..r8-1)
  (_, q) <- block labelSndLoop $ do
    r18 <- load i32 r11
    irComment ["Compare counter < total # of args"]
    r19 <- icmp SLt i1 r18 r8
    br r19 [labelSndLoopBody, labelSndLoopExit]
    pure r18
  block1 labelSndLoopBody $ do
    -- Copy supplied_args[q - r5] to assembled_array[q]
    r20 <- sub i32 q r5
    r21 <- getelementptr1 i8Ptr argAs r20
    r22 <- load i8Ptr r21
    r23 <- getelementptr1 i8Ptr r9 q
    store r22 r23
    irComment ["Increment counter"]
    r24 <- add i32 q (I32 1)
    store r24 r11
    irComment ["Jump back to loop condition"]
    br1 labelSndLoop
  block1 labelSndLoopExit $ do
    r25 <- callg i8Ptr "call_n" [r3, r8, r9]
    ret r25

irFinalize :: IRInterpreter (IRConstruct [IRLine])
irFinalize = do
  interpretFunction
    "closure_finalize"
    (irClosureFinalize arg0 arg1)
    [Label t name | Local t name <- [arg0, arg1]]
 where
  arg0 = Local i8Ptr "f"
  arg1 = Local i8PtrPtr "as"
