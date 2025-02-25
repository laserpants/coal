module Noll.Core.LLVM.IRInstruction.Eval where

import Noll.Core.LLVM.IRInstruction (IRInstr (..))
import Noll.Core.LLVM.IRValue (IRValue (..))

import qualified Noll.Core.Language as Core

irEvalExpr :: Core.Expr Core.Type -> IRInstr IRValue
irEvalExpr =
  undefined
