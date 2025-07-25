{-# LANGUAGE StrictData #-}

module Coal.Kernel.LLVM.IRInterpreter.Artifact (IRInterpreterArtifact (..)) where

import Coal.Kernel.LLVM.IRType (IRType (..))
import Data.ByteString (ByteString)
import Extra (Name)

data IRInterpreterArtifact
  = ArtifactHashMapKey Name
  | ArtifactDataConstructor Name IRType
  | ArtifactMemoizedConstant Name
  | ArtifactCFunctionCall Name IRType [IRType]
  | ArtifactStringLiteral Name ByteString
  | ArtifactBignum Name Integer
  deriving (Show, Eq, Ord)
