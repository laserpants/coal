{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IRInstruction.Eval.Closure.Extend (irClosureExtend) where

import Noll.Core.LLVM.IRInstruction (IRInstr)
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRType (IRType (..))
import Noll.Core.LLVM.IRType.Syntax (i32, i8Ptr, ptr, struct)
import Noll.Core.LLVM.IRValue (IRValue (..))
import TextShow (showt)

s applied = struct (i32 : replicate (3 + applied) i8Ptr)

irClosureExtend :: Int -> IRInstr IRValue
irClosureExtend n = do
  r1 <- iBCast (Local i8Ptr "f") (ptr (TNamed ("closure" <> showt n) (s n)))
  r2 <- iGep (s n) r1 (I32 0) (I32 0)
  iComment ""
  iComment "Argument count"
  iComment ""
  pure r2
