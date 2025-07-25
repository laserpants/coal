{-# LANGUAGE OverloadedStrings #-}

module Noll.Kernel.LLVM.IREval.Malloc (irMalloc) where

import Noll.Kernel.LLVM.IREval.Comment (irCommentBlock)
import Noll.Kernel.LLVM.IRInstruction (IRInstr)
import Noll.Kernel.LLVM.IRInstruction.TH (bitcast, callg, gepsize, ptrtoint)
import Noll.Kernel.LLVM.IRType (IRType (..))
import Noll.Kernel.LLVM.IRType.Syntax (i64, i8Ptr, ptr)
import Noll.Kernel.LLVM.IRValue (IRValue (..))

irMalloc :: IRType -> IRInstr IRValue
irMalloc t = do
  irCommentBlock "gc_malloc" $ do
    r1 <- gepsize t (I32 1)
    r2 <- ptrtoint r1 i64
    r3 <- callg i8Ptr "gc_malloc" [r2]
    bitcast r3 (ptr t)
