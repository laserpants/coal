{-# LANGUAGE StrictData #-}

module Noll.Core.LLVM.IRType (IRType (..)) where

import Noll.Utils (Name)

-- | LLVM IR language types
data IRType
  = -- | Single-bit integer type
    Int1
  | -- | 8-bit integer type
    Int8
  | -- | 32-bit integer type
    Int32
  | -- | 64-bit integer type
    Int64
  | -- | 32-bit floating-point value type
    Float
  | -- | 64-bit floating-point value type
    Double
  | -- | Void type
    Void
  | -- | Function type (return type coupled with a list of formal parameter types)
    Fun IRType [IRType]
  | -- | Pointer type
    Ptr IRType
  | -- | Structure type (a collection of data members)
    Struct [IRType]
  | -- | Type defined at the top level with a name
    Identified Name IRType
  | -- | Array type (elements sequentially arranged in memory)
    Array Int IRType
  deriving (Show, Eq, Ord, Read)
