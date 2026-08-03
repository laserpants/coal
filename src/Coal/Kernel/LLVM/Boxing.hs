{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Type representation and boxing/unboxing for primitive values.

This module handles the mapping between kernel language types and LLVM IR
types, including the boxing and unboxing operations required for uniform
representation in the runtime system.

= Type representation strategy

Primitive types (@int32@, @bool@, etc.) are represented directly in IR as
their corresponding machine types. Complex types (records, data constructors,
functions) are uniformly represented as @ptr@.

= Boxing and unboxing

Boxing converts a primitive value to a heap-allocated pointer. Unboxing
extracts the primitive value from a boxed pointer. These operations interface
with runtime functions (@rtInt32Box@, @rtInt32Unbox@, etc.).
-}
module Coal.Kernel.LLVM.Boxing (
  irTypeRep,
  irValueTypeRep,
  irBox,
  irUnbox,
  irBoxed,
  isIdentityBox,
) where

import LLVM.IR (IROperand, IRType (TDouble, TFloat, TFun, TPtr), i1, i32, i64)

import Coal.Kernel.LLVM.Monad (IRCodegen)
import Coal.Kernel.LLVM.Runtime (callRuntime)
import Coal.Kernel.LLVM.RuntimeDefs
import Coal.Kernel.Language.Expr (Expr)
import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Language.Type.Function (arity)
import Coal.Kernel.Language.Type.HasType (HasType (typeOf), unfoldType)
import Extras.Data.List.NonEmpty (unsnoc)

irTypeRep :: Type -> IRType
irTypeRep t
  | arity t == 0 =
      typeToIRType t
  | otherwise =
      TFun (typeToIRType rty) (fmap typeToIRType argts)
 where
  (argts, rty) =
    unsnoc (unfoldType t)

typeToIRType :: Type -> IRType
typeToIRType =
  \case
    TCon "bool" [] ->
      i1
    TCon "unit" [] ->
      TPtr
    TCon "int32" [] ->
      i32
    TCon "int64" [] ->
      i64
    TCon "float" [] ->
      TFloat
    TCon "double" [] ->
      TDouble
    TCon "char" [] ->
      i32
    TCon "string" [] ->
      TPtr
    TCon "bignum" [] ->
      TPtr
    _ ->
      TPtr

{- | The IR type used when a value of this type is passed at runtime.

Function-typed values are heap-allocated closures, represented as @ptr@.
-}
irValueTypeRep :: Type -> IRType
irValueTypeRep t
  | arity t > 0 =
      TPtr
  | otherwise =
      irTypeRep t

irBox :: Type -> IROperand -> IRCodegen IROperand
irBox t op =
  case t of
    TCon "int32" [] ->
      callRuntime rtInt32Box [op]
    TCon "int64" [] ->
      callRuntime rtInt64Box [op]
    TCon "bool" [] ->
      callRuntime rtBoolBox [op]
    TCon "unit" [] ->
      return op
    TCon "char" [] ->
      callRuntime rtCharBox [op]
    TCon "float" [] ->
      callRuntime rtFloatBox [op]
    TCon "double" [] ->
      callRuntime rtDoubleBox [op]
    TCon "string" [] ->
      return op
    TCon "bignum" [] ->
      return op
    _ ->
      return op

irBoxed :: (Expr Type -> IRCodegen IROperand) -> Expr Type -> IRCodegen IROperand
irBoxed irValue e = irBox (typeOf e) =<< irValue e

irUnbox :: Type -> IROperand -> IRCodegen IROperand
irUnbox t op =
  case t of
    TCon "int32" [] ->
      callRuntime rtInt32Unbox [op]
    TCon "int64" [] ->
      callRuntime rtInt64Unbox [op]
    TCon "bool" [] ->
      callRuntime rtBoolUnbox [op]
    TCon "unit" [] ->
      return op
    TCon "char" [] ->
      callRuntime rtCharUnbox [op]
    TCon "float" [] ->
      callRuntime rtFloatUnbox [op]
    TCon "double" [] ->
      callRuntime rtDoubleUnbox [op]
    TCon "string" [] ->
      return op
    TCon "bignum" [] ->
      return op
    _ ->
      return op

{- | True when 'irBox' / 'irUnbox' are no-ops for this type (value is already @ptr@).

Used to decide whether a call result can be returned via a true LLVM tail call
without a post-call boxing/unboxing step.
-}
isIdentityBox :: Type -> Bool
isIdentityBox =
  \case
    TCon "int32" [] -> False
    TCon "int64" [] -> False
    TCon "bool" [] -> False
    TCon "char" [] -> False
    TCon "float" [] -> False
    TCon "double" [] -> False
    _ -> True
