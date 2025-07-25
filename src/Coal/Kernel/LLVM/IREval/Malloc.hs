{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.LLVM.IREval.Malloc (irMalloc) where

import Coal.Kernel.LLVM.IREval.Comment (irCommentBlock)
import Coal.Kernel.LLVM.IRInstruction (IRInstr)
import Coal.Kernel.LLVM.IRInstruction.TH (bitcast, callg, gepsize, ptrtoint)
import Coal.Kernel.LLVM.IRType (IRType (..))
import Coal.Kernel.LLVM.IRType.Syntax (i64, i8Ptr, ptr)
import Coal.Kernel.LLVM.IRValue (IRValue (..))

irMalloc :: IRType -> IRInstr IRValue
irMalloc t = do
  irCommentBlock "gc_malloc" $ do
    r1 <- gepsize t (I32 1)
    r2 <- ptrtoint r1 i64
    r3 <- callg i8Ptr "gc_malloc" [r2]
    bitcast r3 (ptr t)
