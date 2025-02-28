{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IRInstruction.Interpreter.Artifact (artifactInterpreter) where

import Noll.Core.LLVM.IRConstruct (IRConstruct (..))
import Noll.Core.LLVM.IRInstruction.Interpreter (IRInterpreter (..), IRInterpreterArtifact (..))
import Noll.Core.LLVM.IRType.Syntax (i32, i8Ptr, struct)
import TextShow (showt)

artifactInterpreter :: IRInterpreterArtifact -> IRInterpreter (IRConstruct a)
artifactInterpreter =
  \case
    InterpreterArtifactClosure applied ->
      pure (CType ("closure" <> showt applied) t)
     where
      t = struct (i32 : replicate (3 + applied) i8Ptr)
    _ ->
      error "TODO"
