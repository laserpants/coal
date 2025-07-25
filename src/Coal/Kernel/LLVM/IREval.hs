module Coal.Kernel.LLVM.IREval (IREval (..)) where

import Coal.Kernel.LLVM.IRInstruction (IRInstr)
import Coal.Kernel.LLVM.IRValue (IRValue)

class IREval e where
  irEval :: e -> IRInstr IRValue
