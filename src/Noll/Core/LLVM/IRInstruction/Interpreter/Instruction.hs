{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IRInstruction.Interpreter.Instruction (instruction, instruction1) where

import Control.Monad.RWS (tell)
import Data.Text (Text)
import Noll.Core.LLVM.IREncodable (IREncodable (..))
import Noll.Core.LLVM.IRInstruction.Interpreter.Types (
  IRInterpreter (..),
  IRLine (..),
  nextRegister,
 )
import Noll.Core.LLVM.IRType (IRType (..))
import Noll.Core.LLVM.IRValue (IRValue (..))

instruction :: IRType -> (IRValue -> IRInterpreter a) -> [Text] -> IRInterpreter a
instruction t next tokens = do
  r <- nextRegister t
  tell [LInstruction ([irEncode r, "="] <> tokens)]
  next r

instruction1 :: IRInterpreter a -> [Text] -> IRInterpreter a
instruction1 next tokens = tell [LInstruction tokens] >> next
