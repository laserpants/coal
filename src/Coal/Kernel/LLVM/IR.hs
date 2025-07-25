module Coal.Kernel.LLVM.IR (
  module Coal.Kernel.LLVM.IRInstruction,
  module Coal.Kernel.LLVM.IRType,
  module Coal.Kernel.LLVM.IRValue,
  module Coal.Kernel.LLVM.IRType.Syntax,
  module Coal.Kernel.LLVM.IRConstruct,
  module Coal.Kernel.LLVM.IREval.Expr,
  module Coal.Kernel.LLVM.IRInterpreter,
  module Coal.Kernel.LLVM.IRInstruction.TH,
) where

import Coal.Kernel.LLVM.IRConstruct
import Coal.Kernel.LLVM.IREval.Expr
import Coal.Kernel.LLVM.IRInstruction
import Coal.Kernel.LLVM.IRInstruction.TH
import Coal.Kernel.LLVM.IRInterpreter
import Coal.Kernel.LLVM.IRType
import Coal.Kernel.LLVM.IRType.Syntax
import Coal.Kernel.LLVM.IRValue
