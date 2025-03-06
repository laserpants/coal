{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IREval.Malloc (irMalloc) where

import Noll.Core.LLVM.IREval.Comment (irCommentBlock)
import Noll.Core.LLVM.IRInstruction (IRInstr)
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRType (IRType (..))
import Noll.Core.LLVM.IRType.Syntax (i64, i8Ptr, ptr)
import Noll.Core.LLVM.IRValue (IRValue (..))

irMalloc :: IRType -> IRInstr IRValue
irMalloc t = do
  irCommentBlock "gc_malloc" $ do
    r1 <- iGepNull (ptr t) (I32 1)
    r2 <- iPtrtoint r1 i64
    r3 <- iCallGlobal i8Ptr "gc_malloc" [r2]
    iBCast r3 (ptr t)
