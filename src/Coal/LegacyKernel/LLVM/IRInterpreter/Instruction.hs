{-# LANGUAGE OverloadedStrings #-}

module Coal.LegacyKernel.LLVM.IRInterpreter.Instruction (instruction, instruction1) where

import Coal.LegacyKernel.LLVM.IREncodable (IREncodable (..))
import Coal.LegacyKernel.LLVM.IRInterpreter.Monad (IRInterpreter (..), IRLine (..), nextRegister)
import Coal.LegacyKernel.LLVM.IRType (IRType (..))
import Coal.LegacyKernel.LLVM.IRValue (IRValue (..))
import Control.Monad.RWS (tell)
import Data.Text (Text)

instruction :: IRType -> (IRValue -> IRInterpreter a) -> [Text] -> IRInterpreter a
instruction t next tokens = do
  r <- nextRegister t
  tell [LInstruction ([irEncode r, "="] <> tokens)]
  next r

instruction1 :: IRInterpreter a -> [Text] -> IRInterpreter a
instruction1 next tokens = tell [LInstruction tokens] >> next
