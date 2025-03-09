{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IRInstruction.Interpreter.Artifact (interpretArtifact) where

import Data.Text.Encoding (encodeUtf8)
import Noll.Core.LLVM.IRConstruct (IRConstruct (..), IRLinkage (..))
import Noll.Core.LLVM.IREval.Closure (structType)
import Noll.Core.LLVM.IRInstruction.Interpreter (IRInterpreter (..), IRLine (..))
import Noll.Core.LLVM.IRInstruction.Interpreter.State
import Noll.Core.LLVM.IRType.Syntax (i8Ptr)
import Noll.Core.LLVM.IRValue (IRValue (..))
import TextShow (showt)

interpretArtifact :: IRInterpreterArtifact -> IRInterpreter [IRConstruct [IRLine]]
interpretArtifact =
  \case
--    InterpreterArtifactClosure argc ->
--      pure [CType ("closure" <> showt argc) (structType argc)]
    InterpreterArtifactHashMapKey name ->
      pure [CString ("label." <> name) (encodeUtf8 name)]
    InterpreterArtifactDataConstructor name t ->
      pure [CType name t]
    InterpreterArtifactMemoizedConstant name ->
      pure [CGlobal name i8Ptr (Just LPrivate) Null]
    _ ->
      pure []
