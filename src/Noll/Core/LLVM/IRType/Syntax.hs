module Noll.Core.LLVM.IRType.Syntax (
  i1,
  i8,
  i32,
  i64,
  ptr,
  i8Ptr,
  i8PtrPtr,
  fun,
  struct,
  stringLiteralType,
  opaqueIRSignature,
) where

import Data.Text (Text)
import Noll.Core.LLVM.IRType (IRType (..))

import qualified Data.Text as Text

{-# INLINE i1 #-}
i1 :: IRType
i1 = TInt1

{-# INLINE i8 #-}
i8 :: IRType
i8 = TInt8

{-# INLINE i32 #-}
i32 :: IRType
i32 = TInt32

{-# INLINE i64 #-}
i64 :: IRType
i64 = TInt64

{-# INLINE ptr #-}
ptr :: IRType -> IRType
ptr = TPtr

{-# INLINE i8Ptr #-}
i8Ptr :: IRType
i8Ptr = ptr i8

{-# INLINE i8PtrPtr #-}
i8PtrPtr :: IRType
i8PtrPtr = ptr (ptr i8)

{-# INLINE fun #-}
fun :: IRType -> [IRType] -> IRType
fun = TFun

{-# INLINE struct #-}
struct :: [IRType] -> IRType
struct = TStruct

{-# INLINE stringLiteralType #-}
stringLiteralType :: Int -> IRType
stringLiteralType len = TArray len i8

{-# INLINE opaqueIRSignature #-}
opaqueIRSignature :: Int -> IRType
opaqueIRSignature n = fun i8Ptr (replicate n i8Ptr)
