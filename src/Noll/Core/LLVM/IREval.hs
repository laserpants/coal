module Noll.Core.LLVM.IREval (IREval (..), irEvalArgs, irEvalFun) where

import Noll.Common.List1 (List1, fromList1)
import Noll.Core.LLVM.IRInstruction (IRInstr)
import Noll.Core.LLVM.IRInstruction.TH (iRet)
import Noll.Core.LLVM.IRType.Syntax (i8Ptr)
import Noll.Core.LLVM.IRValue (IRValue)

class IREval e where
  irEval :: e -> IRInstr IRValue

{-# INLINE irEvalArgs #-}
irEvalArgs :: (IREval e) => List1 e -> IRInstr [IRValue]
irEvalArgs = mapM irEval . fromList1

{-# INLINE irEvalFun #-}
irEvalFun :: (IREval e) => e -> IRInstr ()
irEvalFun e = irEval e >>= iRet i8Ptr
