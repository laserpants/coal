{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Kernel.Translate.Operator (translateOperator) where

import Coal.Common.Label (Label (..))
import Coal.Compiler.Kernel.Translate.Type (translateType)
import Coal.Compiler.Stack (CompilerT)
import Coal.LegacyKernel.Compiler (KernelExpr)
import qualified Coal.LegacyKernel.Language as Kernel
import Coal.Language
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))

logicalNotOperator :: (Monad m) => (Expression a Kind IndexedType -> CompilerT a m KernelExpr) -> NonEmpty (Expression a Kind IndexedType) -> CompilerT a m KernelExpr
logicalNotOperator translate es = do
  args <- traverse translate es
  pure $
    Kernel.app
      t1
      (Kernel.var (Label (t1 `Kernel.arrow` t1) "Builtin$.operator$__not"))
      args
 where
  t1 = translateType (TIntrinsic IBool)

translateOperator :: (Monad m, Data a) => (Expression a Kind IndexedType -> CompilerT a m KernelExpr) -> IndexedType -> Operator -> NonEmpty (Expression a Kind IndexedType) -> CompilerT a m KernelExpr
translateOperator translate t =
  \case
    OLogicalNot ->
      logicalNotOperator translate
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

stringConcatenationOperator :: (Monad m) => (Expression a Kind IndexedType -> CompilerT a m KernelExpr) -> NonEmpty (Expression a Kind IndexedType) -> CompilerT a m KernelExpr
stringConcatenationOperator translate es = do
  args <- traverse translate es
  let t1 = translateType (TIntrinsic IString)
  pure $
    Kernel.app
      t1
      (Kernel.var (Label (t1 `Kernel.arrow` t1 `Kernel.arrow` t1) "Builtin$.operator$__string_concatenation"))
      args

listConcatenationOperator :: (Monad m) => (Expression a Kind IndexedType -> CompilerT a m KernelExpr) -> IndexedType -> NonEmpty (Expression a Kind IndexedType) -> CompilerT a m KernelExpr
listConcatenationOperator translate t es = do
  args <- traverse translate es
  let t1 = translateType t
  pure $
    Kernel.app
      t1
      (Kernel.var (Label (t1 `Kernel.arrow` t1 `Kernel.arrow` t1) "Builtin$.operator$__list_concatenation"))
      args

reverseCompositionOperator :: (Monad m) => (Expression a Kind IndexedType -> CompilerT a m KernelExpr) -> IndexedType -> NonEmpty (Expression a Kind IndexedType) -> CompilerT a m KernelExpr
reverseCompositionOperator translate t es = do
  args <- traverse translate es
  let t1 = translateType t
  pure $
    Kernel.app
      t1
      (Kernel.var (Label (Kernel.foldType t1 (Kernel.typeOf <$> args)) "Builtin$.operator$__reverse_composition"))
      args

forwardCompositionOperator :: (Monad m) => (Expression a Kind IndexedType -> CompilerT a m KernelExpr) -> IndexedType -> NonEmpty (Expression a Kind IndexedType) -> CompilerT a m KernelExpr
forwardCompositionOperator translate t es = do
  args <- traverse translate es
  let t1 = translateType t
  pure $
    Kernel.app
      t1
      (Kernel.var (Label (Kernel.foldType t1 (Kernel.typeOf <$> args)) "Builtin$.operator$__forward_composition"))
      args

reverseApplicationOperator :: (Monad m) => (Expression a Kind IndexedType -> CompilerT a m KernelExpr) -> IndexedType -> NonEmpty (Expression a Kind IndexedType) -> CompilerT a m KernelExpr
reverseApplicationOperator translate t es = do
  args <- traverse translate es
  let t1 = translateType t
  pure $
    Kernel.app
      t1
      (Kernel.var (Label (Kernel.foldType t1 (Kernel.typeOf <$> args)) "Builtin$.operator$__reverse_application"))
      args

forwardApplicationOperator :: (Monad m) => (Expression a Kind IndexedType -> CompilerT a m KernelExpr) -> IndexedType -> NonEmpty (Expression a Kind IndexedType) -> CompilerT a m KernelExpr
forwardApplicationOperator translate t es = do
  args <- traverse translate es
  let t1 = translateType t
  pure $
    Kernel.app
      t1
      (Kernel.var (Label (Kernel.foldType t1 (Kernel.typeOf <$> args)) "Builtin$.operator$__forward_application"))
      args

binop :: (Monad m, Data a) => (Expression a Kind IndexedType -> CompilerT a m KernelExpr) -> (KernelExpr -> KernelExpr -> Kernel.Op KernelExpr) -> (IndexedType, IndexedType) -> NonEmpty (Expression a Kind IndexedType) -> CompilerT a m KernelExpr
binop translate op (t1, t2) (e1 :| [e2])
  | e1 `hasType` t1 && e2 `hasType` t2 = do
      o1 <- translate e1
      o2 <- translate e2
      pure (Kernel.op (op o1 o2))
binop _ _ _ _ = error "Implementation error"

{-# INLINE hasType #-}
hasType :: (Data a) => Expression a Kind IndexedType -> IndexedType -> Bool
hasType e t = typeOf e == t
