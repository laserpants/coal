{-# LANGUAGE OverloadedStrings #-}

module Noll.Kernel.LLVM.IRInterpreter.Instruction (instruction, instruction1) where

import Control.Monad.RWS (tell)
import Data.Text (Text)
import Noll.Kernel.LLVM.IREncodable (IREncodable (..))
import Noll.Kernel.LLVM.IRInterpreter.Monad (IRInterpreter (..), IRLine (..), nextRegister)
import Noll.Kernel.LLVM.IRType (IRType (..))
import Noll.Kernel.LLVM.IRValue (IRValue (..))

instruction :: IRType -> (IRValue -> IRInterpreter a) -> [Text] -> IRInterpreter a
instruction t next tokens = do
  r <- nextRegister t
  tell [LInstruction ([irEncode r, "="] <> tokens)]
  next r

instruction1 :: IRInterpreter a -> [Text] -> IRInterpreter a
instruction1 next tokens = tell [LInstruction tokens] >> next
