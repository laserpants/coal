{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.LLVM.IRType (
  IRType (..),
  IRTyped (..),
  pointee,
) where

import Noll.Core.Language.Expr (Expr)
import Noll.Core.Language.Type (Type (..))
import Noll.Core.Language.Typed (Typed (..))
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

instance IRTyped Type where
  irTypeOf =
    \case
      TCon "unit" [] ->
        TInt1
      TCon "bool" [] ->
        TInt1
      TCon "int32" [] ->
        TInt32
      TCon "int64" [] ->
        TInt64
      TCon "float" [] ->
        TFloat
      TCon "double" [] ->
        TDouble
      TCon "char" [] ->
        TInt32
      TCon "string" [] ->
        TPtr TInt8
      _ ->
        TPtr TInt8

instance IRTyped (Expr Type) where
  irTypeOf = irTypeOf . typeOf

pointee :: IRType -> Maybe IRType
pointee =
  \case
    TPtr t ->
      Just t
    _ ->
      Nothing
