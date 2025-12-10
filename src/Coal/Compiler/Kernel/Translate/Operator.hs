{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Kernel.Translate.Operator (
  translateUnaryOperator,
  translateBinaryOperator,
) where

import Coal.Common.Label (Label (..))
import Coal.Compiler.Kernel.Translate.Type (translateType)
import Coal.Compiler.Stack (CompilerT)
import Coal.Kernel.Compiler (KernelExpr)
import qualified Coal.Kernel.Language as Kernel
import Coal.Language
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))

translateUnaryOperator :: (Monad m) => (Expression a IndexedType -> CompilerT a m KernelExpr) -> IndexedType -> UnaryOperator -> NonEmpty (Expression a IndexedType) -> CompilerT a m KernelExpr
translateUnaryOperator translate _ =
  \case
    OLogicalNot ->
      logicalNotOperator translate
    ONegate{} ->
      error "Not implemented"

logicalNotOperator :: (Monad m) => (Expression a IndexedType -> CompilerT a m KernelExpr) -> NonEmpty (Expression a IndexedType) -> CompilerT a m KernelExpr
logicalNotOperator translate es = do
  args <- traverse translate es
  pure $
    Kernel.app
      t1
      (Kernel.var (Label (t1 `Kernel.arrow` t1) "Builtin$.operator$__not"))
      args
 where
  t1 = translateType (TIntrinsic IBool)

translateBinaryOperator :: (Monad m, Data a) => (Expression a IndexedType -> CompilerT a m KernelExpr) -> IndexedType -> IndexedType -> BinaryOperator -> NonEmpty (Expression a IndexedType) -> CompilerT a m KernelExpr
translateBinaryOperator translate t ot =
  \case
    OReverseComposition ->
      reverseCompositionOperator translate t
    OReverseApplication ->
      reverseApplicationOperator translate t
    OListConcatenation ->
      listConcatenationOperator translate t
    OLessThan ->
      binop translate Kernel.OLtInt32 (TIntrinsic IInt32, TIntrinsic IInt32)
    OGreaterThan ->
      binop translate Kernel.OGtInt32 (TIntrinsic IInt32, TIntrinsic IInt32)
    OLessThanOrEqual ->
      binop translate Kernel.OLteInt32 (TIntrinsic IInt32, TIntrinsic IInt32)
    OGreaterThanOrEqual ->
      binop translate Kernel.OGteInt32 (TIntrinsic IInt32, TIntrinsic IInt32)
    OLogicalAnd ->
      binop translate Kernel.OAnd (TIntrinsic IBool, TIntrinsic IBool)
    OLogicalOr ->
      binop translate Kernel.OOr (TIntrinsic IBool, TIntrinsic IBool)
    OAddition
      | TIntrinsic IInt32 == t ->
          binop translate Kernel.OAddInt32 (TIntrinsic IInt32, TIntrinsic IInt32)
    OAddition
      | TIntrinsic IInt64 == t ->
          binop translate Kernel.OAddInt64 (TIntrinsic IInt64, TIntrinsic IInt64)
    OAddition
      | TIntrinsic IFloat == t ->
          binop translate Kernel.OAddFloat (TIntrinsic IFloat, TIntrinsic IFloat)
    OAddition
      | TIntrinsic IDouble == t ->
          binop translate Kernel.OAddDouble (TIntrinsic IDouble, TIntrinsic IDouble)
    OSubtraction
      | TIntrinsic IInt32 == t ->
          binop translate Kernel.OSubInt32 (TIntrinsic IInt32, TIntrinsic IInt32)
    OSubtraction
      | TIntrinsic IInt64 == t ->
          binop translate Kernel.OSubInt64 (TIntrinsic IInt64, TIntrinsic IInt64)
    OSubtraction
      | TIntrinsic IFloat == t ->
          binop translate Kernel.OSubFloat (TIntrinsic IFloat, TIntrinsic IFloat)
    OSubtraction
      | TIntrinsic IDouble == t ->
          binop translate Kernel.OSubDouble (TIntrinsic IDouble, TIntrinsic IDouble)
    OMultiplication
      | TIntrinsic IInt32 == t ->
          binop translate Kernel.OMulInt32 (TIntrinsic IInt32, TIntrinsic IInt32)
    OMultiplication
      | TIntrinsic IInt64 == t ->
          binop translate Kernel.OMulInt64 (TIntrinsic IInt64, TIntrinsic IInt64)
    OMultiplication
      | TIntrinsic IFloat == t ->
          binop translate Kernel.OMulFloat (TIntrinsic IFloat, TIntrinsic IFloat)
    OMultiplication
      | TIntrinsic IDouble == t ->
          binop translate Kernel.OMulDouble (TIntrinsic IDouble, TIntrinsic IDouble)
    ODivision
      | TIntrinsic IInt32 == t ->
          binop translate Kernel.ODivInt32 (TIntrinsic IInt32, TIntrinsic IInt32)
    ODivision
      | TIntrinsic IInt64 == t ->
          binop translate Kernel.ODivInt64 (TIntrinsic IInt64, TIntrinsic IInt64)
    ODivision
      | TIntrinsic IFloat == t ->
          binop translate Kernel.ODivFloat (TIntrinsic IFloat, TIntrinsic IFloat)
    ODivision
      | TIntrinsic IDouble == t ->
          binop translate Kernel.ODivDouble (TIntrinsic IDouble, TIntrinsic IDouble)
    OStringConcatenation ->
      stringConcatenationOperator translate
    OEqualTo ->
      equalityOperator translate ot
    _ ->
      error "Not implemented"

equalityOperator :: (Monad m) => (Expression a IndexedType -> CompilerT a m KernelExpr) -> IndexedType -> NonEmpty (Expression a IndexedType) -> CompilerT a m KernelExpr
equalityOperator translate ot (e1 :| [e2]) = do
  o1 <- translate e1
  o2 <- translate e2
  case ot of
    (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IBool) ->
      pure (Kernel.op (Kernel.OEqInt32 o1 o2))
    (TIntrinsic IInt64 `TArrow` TIntrinsic IInt64 `TArrow` TIntrinsic IBool) ->
      pure (Kernel.op (Kernel.OEqInt64 o1 o2))
    (TIntrinsic IFloat `TArrow` TIntrinsic IFloat `TArrow` TIntrinsic IBool) ->
      pure (Kernel.op (Kernel.OEqFloat o1 o2))
    (TIntrinsic IDouble `TArrow` TIntrinsic IDouble `TArrow` TIntrinsic IBool) ->
      pure (Kernel.op (Kernel.OEqDouble o1 o2))
    (TIntrinsic IChar `TArrow` TIntrinsic IChar `TArrow` TIntrinsic IBool) ->
      pure (Kernel.op (Kernel.OEqChar o1 o2))
    (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) ->
      pure (Kernel.op (Kernel.OEqBool o1 o2))
    _ ->
      error "Not implemented"
equalityOperator _ _ _ = error "Not implemented"

stringConcatenationOperator :: (Monad m) => (Expression a IndexedType -> CompilerT a m KernelExpr) -> NonEmpty (Expression a IndexedType) -> CompilerT a m KernelExpr
stringConcatenationOperator translate es = do
  args <- traverse translate es
  let t1 = translateType (TIntrinsic IString)
  pure $
    Kernel.app
      t1
      (Kernel.var (Label (t1 `Kernel.arrow` t1 `Kernel.arrow` t1) "Builtin$.operator$__string_concatenation"))
      args

listConcatenationOperator :: (Monad m) => (Expression a IndexedType -> CompilerT a m KernelExpr) -> IndexedType -> NonEmpty (Expression a IndexedType) -> CompilerT a m KernelExpr
listConcatenationOperator translate t es = do
  args <- traverse translate es
  let t1 = translateType t
  pure $
    Kernel.app
      t1
      (Kernel.var (Label (t1 `Kernel.arrow` t1 `Kernel.arrow` t1) "Builtin$.operator$__list_concatenation"))
      args

reverseCompositionOperator :: (Monad m) => (Expression a IndexedType -> CompilerT a m KernelExpr) -> IndexedType -> NonEmpty (Expression a IndexedType) -> CompilerT a m KernelExpr
reverseCompositionOperator translate t es = do
  args <- traverse translate es
  let t1 = translateType t
  pure $
    Kernel.app
      t1
      (Kernel.var (Label (Kernel.foldType t1 (Kernel.typeOf <$> args)) "Builtin$.operator$__reverse_composition"))
      args

reverseApplicationOperator :: (Monad m) => (Expression a IndexedType -> CompilerT a m KernelExpr) -> IndexedType -> NonEmpty (Expression a IndexedType) -> CompilerT a m KernelExpr
reverseApplicationOperator translate t es = do
  args <- traverse translate es
  let t1 = translateType t
  pure $
    Kernel.app
      t1
      (Kernel.var (Label (Kernel.foldType t1 (Kernel.typeOf <$> args)) "Builtin$.operator$__reverse_application"))
      args

binop :: (Monad m, Data a) => (Expression a IndexedType -> CompilerT a m KernelExpr) -> (KernelExpr -> KernelExpr -> Kernel.Op KernelExpr) -> (IndexedType, IndexedType) -> NonEmpty (Expression a IndexedType) -> CompilerT a m KernelExpr
binop translate op (t1, t2) (e1 :| [e2])
  | e1 `hasType` t1 && e2 `hasType` t2 = do
      o1 <- translate e1
      o2 <- translate e2
      pure (Kernel.op (op o1 o2))
binop _ _ _ _ = error "Implementation error"

{-# INLINE hasType #-}
hasType :: (Data a) => Expression a IndexedType -> IndexedType -> Bool
hasType e t = typeOf e == t
