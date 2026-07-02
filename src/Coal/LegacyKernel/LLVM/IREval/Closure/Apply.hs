{-# LANGUAGE OverloadedStrings #-}

module Coal.LegacyKernel.LLVM.IREval.Closure.Apply (irApply) where

import Coal.Common.Label (Label (..))
import Coal.LegacyKernel.LLVM.IRConstruct (IRConstruct (..))
import Coal.LegacyKernel.LLVM.IREval.Closure (closureStructType)
import Coal.LegacyKernel.LLVM.IREval.Comment (irComment)
import Coal.LegacyKernel.LLVM.IRInstruction (ICmpCond (..), IRInstr)
import Coal.LegacyKernel.LLVM.IRInstruction.Builders
import Coal.LegacyKernel.LLVM.IRInterpreter (interpretFunction)
import Coal.LegacyKernel.LLVM.IRInterpreter.Monad (IRInterpreter, IRLine)
import Coal.LegacyKernel.LLVM.IRType.Syntax (i1, i32, i8Ptr, i8PtrPtr, ptr)
import Coal.LegacyKernel.LLVM.IRValue (IRValue (..))

irClosureApply :: IRValue -> IRValue -> IRValue -> IRInstr ()
irClosureApply argF argN argAs = do
  let t = closureStructType 0
  r1 <- bitcast argF (ptr t)
  r2 <- getelementptr t r1 (I32 0) (I32 1)
  irComment ["Remaining argument count"]
  r3 <- load i32 r2
  irComment ["Overflow (supplied # args - expected # args)"]
  r4 <- sub i32 argN r3
  labelExactMatch <- label "exact_match"
  labelNotZero <- label "not_zero"
  isZ <- icmp Eq i1 r4 (I32 0)
  br isZ [labelExactMatch, labelNotZero]
  block1 labelNotZero $ do
    labelUnder <- label "under"
    labelOver <- label "over"
    isNeg <- icmp SLt i1 r4 (I32 0)
    br isNeg [labelUnder, labelOver]
    irComment ["Function is undersaturated"]
    block1 labelUnder $ do
      r7 <- callg i8Ptr "closure_extend" [argF, argN, argAs]
      ret r7
    irComment ["Function is oversaturated"]
    block1 labelOver $ do
      r8 <- callg i8Ptr "closure_finalize" [argF, argAs]
      r9 <- getelementptr1 i8Ptr argAs r3
      r10 <- callg i8Ptr "apply" [r8, r4, r9]
      ret r10
  irComment ["Number of supplied arguments matches function signature"]
  block1 labelExactMatch $ do
    r11 <- callg i8Ptr "closure_finalize" [argF, argAs]
    ret r11

irApply :: IRInterpreter (IRConstruct [IRLine])
irApply =
  interpretFunction
    "apply"
    (irClosureApply argF argN argAs)
    [Label t name | Local t name <- [argF, argN, argAs]]
 where
  argF = Local i8Ptr "f"
  argN = Local i32 "n"
  argAs = Local i8PtrPtr "as"
