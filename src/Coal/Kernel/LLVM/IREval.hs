module Coal.Kernel.LLVM.IREval (
  IREval (..),
  IRTailContext (..),
) where

import Coal.Kernel.LLVM.IRInstruction (IRInstr)
import Coal.Kernel.LLVM.IRValue (IRValue)

-- | Tail context for tracking whether an expression is in tail position
data IRTailContext
  = InTail -- Expression is in tail position (last operation before return)
  | NotInTail -- Expression is not in tail position
  deriving (Show, Eq, Ord)

class IREval e where
  irEval :: IRTailContext -> e -> IRInstr IRValue
