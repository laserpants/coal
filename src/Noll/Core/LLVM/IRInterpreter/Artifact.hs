{-# LANGUAGE StrictData #-}

module Noll.Core.LLVM.IRInterpreter.Artifact (IRInterpreterArtifact (..)) where

import Noll.Core.LLVM.IRType (IRType (..))
import Noll.Utils (Name)

data IRInterpreterArtifact
  = InterpreterArtifactFunctionApply Int
  | InterpreterArtifactClosure Int
  | InterpreterArtifactHashMapKey Name
  | InterpreterArtifactDataConstructor Name IRType
  | InterpreterArtifactMemoizedConstant Name
  deriving (Show, Eq, Ord)
