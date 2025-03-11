{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IRInterpreter.Instruction where

import Control.Monad.RWS (MonadReader, MonadState, MonadWriter, RWS, evalRWS, gets, runRWS, tell)
import Data.Text (Text)
import Noll.Core.LLVM.IREncodable (IRAnnotated (..), IREncodable (..), annotated, commaSep, encodeLabel, enquote, irGlobalName, irLocalName)
import Noll.Core.LLVM.IRInterpreter.Monad (IRInterpreter (..), IRLine (..), nextRegister)
import Noll.Core.LLVM.IRType (IRType (..), IRTyped (..))
import Noll.Core.LLVM.IRValue (IRValue (..))

instruction :: IRType -> (IRValue -> IRInterpreter a) -> [Text] -> IRInterpreter a
instruction t next tokens = do
  r <- nextRegister t
  tell [LInstruction ([irEncode r, "="] <> tokens)]
  next r

instruction1 :: IRInterpreter a -> [Text] -> IRInterpreter a
instruction1 next tokens = tell [LInstruction tokens] >> next
