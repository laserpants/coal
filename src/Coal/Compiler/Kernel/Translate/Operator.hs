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
    OForwardApplication ->
      forwardApplicationOperator translate t
    OForwardComposition ->
      forwardCompositionOperator translate t
    OListConcatenation ->
      listConcatenationOperator translate t
    OLogicalAnd ->
      binop translate Kernel.OAnd (TIntrinsic IBool, TIntrinsic IBool)
    OLogicalOr ->
      binop translate Kernel.OOr (TIntrinsic IBool, TIntrinsic IBool)
    OStringConcatenation ->
      stringConcatenationOperator translate

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

forwardCompositionOperator :: (Monad m) => (Expression a IndexedType -> CompilerT a m KernelExpr) -> IndexedType -> NonEmpty (Expression a IndexedType) -> CompilerT a m KernelExpr
forwardCompositionOperator translate t es = do
  args <- traverse translate es
  let t1 = translateType t
  pure $
    Kernel.app
      t1
      (Kernel.var (Label (Kernel.foldType t1 (Kernel.typeOf <$> args)) "Builtin$.operator$__forward_composition"))
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

forwardApplicationOperator :: (Monad m) => (Expression a IndexedType -> CompilerT a m KernelExpr) -> IndexedType -> NonEmpty (Expression a IndexedType) -> CompilerT a m KernelExpr
forwardApplicationOperator translate t es = do
  args <- traverse translate es
  let t1 = translateType t
  pure $
    Kernel.app
      t1
      (Kernel.var (Label (Kernel.foldType t1 (Kernel.typeOf <$> args)) "Builtin$.operator$__forward_application"))
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
