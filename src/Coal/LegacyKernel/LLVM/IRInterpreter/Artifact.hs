{-# LANGUAGE StrictData #-}

module Coal.LegacyKernel.LLVM.IRInterpreter.Artifact (IRInterpreterArtifact (..)) where

import Coal.LegacyKernel.LLVM.IRType (IRType (..))
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
