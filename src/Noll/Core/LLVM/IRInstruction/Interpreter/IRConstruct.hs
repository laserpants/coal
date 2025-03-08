module Noll.Core.LLVM.IRInstruction.Interpreter.IRConstruct (irDefine) where

import Noll.Core.LLVM.IRConstruct (IRConstruct (..))
import Noll.Core.LLVM.IRInstruction (IRInstr)
import Noll.Core.LLVM.IRInstruction.Interpreter (IRInterpreter (..), IRLine (..), interpret)
import Noll.Core.LLVM.IRType (IRType (..))
import Noll.Core.LLVM.IRType.Syntax (i8Ptr)
import Noll.Label (Label (..))
import Noll.Utils (Name, listenOnly)

irDefine :: Name -> IRInstr a -> [Label IRType] -> IRInterpreter (IRConstruct [IRLine])
irDefine name f args = CDefine name i8Ptr Nothing args <$> listenOnly (interpret f)
