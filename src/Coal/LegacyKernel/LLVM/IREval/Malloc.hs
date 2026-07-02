{-# LANGUAGE OverloadedStrings #-}

module Coal.LegacyKernel.LLVM.IREval.Malloc (irMalloc, irMallocN) where

import Coal.LegacyKernel.LLVM.IREval.Comment (irCommentBlock)
import Coal.LegacyKernel.LLVM.IRInstruction (IRInstr)
import Coal.LegacyKernel.LLVM.IRInstruction.Builders (bitcast, callg, gepsize, ptrtoint)
import Coal.LegacyKernel.LLVM.IRType (IRType (..))
import Coal.LegacyKernel.LLVM.IRType.Syntax (i64, i8Ptr, ptr)
import Coal.LegacyKernel.LLVM.IRValue (IRValue (..))

irMalloc :: IRType -> IRInstr IRValue
irMalloc t = do
  irCommentBlock "rt_alloc" $ do
    r1 <- gepsize t (I32 1)
    r2 <- ptrtoint r1 i64
    r3 <- callg i8Ptr "rt_alloc" [r2]
    bitcast r3 (ptr t)

irMallocN :: IRType -> IRValue -> IRInstr IRValue
irMallocN t n = do
  irCommentBlock "rt_alloc" $ do
    r1 <- gepsize t n
    r2 <- ptrtoint r1 i64
    r3 <- callg i8Ptr "rt_alloc" [r2]
    bitcast r3 (ptr t)
