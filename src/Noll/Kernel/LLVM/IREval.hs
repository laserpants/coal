module Noll.Kernel.LLVM.IREval (IREval (..)) where

import Noll.Kernel.LLVM.IRInstruction (IRInstr)
import Noll.Kernel.LLVM.IRValue (IRValue)

class IREval e where
  irEval :: e -> IRInstr IRValue
