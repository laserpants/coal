{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IRInstruction.Interpreter.Artifact (interpretArtifact) where

import Noll.Core.LLVM.IRConstruct (IRConstruct (..))
import Noll.Core.LLVM.IREval.Closure (structType)
import Noll.Core.LLVM.IRInstruction.Interpreter (IRInterpreter (..), IRLine (..))
import Noll.Core.LLVM.IRInstruction.Interpreter.State
import Noll.Core.LLVM.IRType.Syntax (i8Ptr)
import Noll.Core.LLVM.IRValue (IRValue (..))
import TextShow (showt)

interpretArtifact :: IRInterpreterArtifact -> IRInterpreter [IRConstruct [IRLine]]
interpretArtifact =
  \case
    InterpreterArtifactClosure arity ->
      pure [CType ("closure" <> showt arity) (structType arity)]
    InterpreterArtifactHashMapKey name ->
      pure [CString ("label_" <> name) name]
    InterpreterArtifactDataConstructor name t ->
      pure [CType name t]
    InterpreterArtifactMemoizedConstant name ->
      pure [CGlobal name i8Ptr Null]
    _ ->
      pure []
