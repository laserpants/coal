{-# LANGUAGE StrictData #-}

module Coal.Kernel.LLVM.IRInterpreter.Artifact (IRInterpreterArtifact (..)) where

import Coal.Kernel.LLVM.IRType (IRType (..))
import Data.ByteString (ByteString)
import Extras (Name)

data IRInterpreterArtifact
  = AHashMapKey Name
  | ADataConstructor Name IRType
  | AMemoizedConstant Name
  | ACFunctionCall Name IRType [IRType]
  | AStringLiteral Name ByteString
  | ABignum Name Integer
  deriving (Show, Eq, Ord)
