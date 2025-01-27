{-# LANGUAGE StrictData #-}

module Noll.Core.LLVM.IRType (IRType (..), IRTyped (..)) where

import Noll.Utils (Name)

-- | LLVM IR language types
data IRType
  = -- | Single-bit integer type
    TInt1
  | -- | 8-bit integer type
    TInt8
  | -- | 32-bit integer type
    TInt32
  | -- | 64-bit integer type
    TInt64
  | -- | 32-bit floating-point value type
    TFloat
  | -- | 64-bit floating-point value type
    TDouble
  | -- | Void type
    TVoid
  | -- | Function type (return type coupled with a list of formal parameter types)
    TFun IRType [IRType]
  | -- | Pointer type
    TPtr IRType
  | -- | Structure type (a collection of data members)
    TStruct [IRType]
  | -- | Named type defined at the top level
    TNamed Name IRType
  | -- | Array type (elements sequentially arranged in memory)
    TArray Int IRType
  deriving (Show, Eq, Ord, Read)

class IRTyped t where
  irTypeOf :: t -> IRType

instance IRTyped IRType where
  irTypeOf = id
