{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.LLVM.IRType (
  IRType (..),
  IRTyped (..),
  i1,
  i8,
  i32,
  i64,
  ptr,
  i8Ptr,
  fun,
  pointee,
  struct,
  stringLiteralType,
) where

import Data.Text (Text)
import Noll.Core.Language.Type (Type (..))
import Noll.Utils (Name)

import qualified Data.Text as Text

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

instance IRTyped Type where
  irTypeOf =
    \case
      TCon "unit" [] ->
        i1
      TCon "bool" [] ->
        i1
      TCon "int32" [] ->
        i32
      TCon "int64" [] ->
        i64
      TCon "float" [] ->
        TFloat
      TCon "double" [] ->
        TDouble
      TCon "char" [] ->
        error "TODO"
      TCon "string" [] ->
        error "TODO"
      _ ->
        i8Ptr

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

{-# INLINE fun #-}
fun :: IRType -> [IRType] -> IRType
fun = TFun

{-# INLINE struct #-}
struct :: [IRType] -> IRType
struct = TStruct

{-# INLINE stringLiteralType #-}
stringLiteralType :: Text -> IRType
stringLiteralType str = TArray (Text.length str + 1) i8

pointee :: IRType -> IRType
pointee =
  \case
    TPtr t ->
      t
    _ ->
      error "Implementation error"
