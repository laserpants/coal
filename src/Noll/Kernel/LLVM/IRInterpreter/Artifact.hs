{-# LANGUAGE StrictData #-}

module Noll.Kernel.LLVM.IRInterpreter.Artifact (IRInterpreterArtifact (..)) where

import Data.ByteString (ByteString)
import Noll.Kernel.LLVM.IRType (IRType (..))
import Extra (Name)

data IRInterpreterArtifact
  = ArtifactHashMapKey Name
  | ArtifactDataConstructor Name IRType
  | ArtifactMemoizedConstant Name
  | ArtifactCFunctionCall Name IRType [IRType]
  | ArtifactStringLiteral Name ByteString
  | ArtifactBignum Name Integer
  deriving (Show, Eq, Ord)
