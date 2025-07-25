{-# LANGUAGE StrictData #-}

module Coal.Kernel.LLVM.IRInterpreter.Artifact (IRInterpreterArtifact (..)) where

import Data.ByteString (ByteString)
import Extra (Name)
import Coal.Kernel.LLVM.IRType (IRType (..))

data IRInterpreterArtifact
  = ArtifactHashMapKey Name
  | ArtifactDataConstructor Name IRType
  | ArtifactMemoizedConstant Name
  | ArtifactCFunctionCall Name IRType [IRType]
  | ArtifactStringLiteral Name ByteString
  | ArtifactBignum Name Integer
  deriving (Show, Eq, Ord)
