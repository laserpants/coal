{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Kernel.Translate.Operator (translateOperator) where

import Coal.Compiler.Kernel.Translate.Type (translateType)
import Coal.Compiler.Stack (CompilerT)
import Coal.Kernel.Language.Expr (Expr (..), Label (..))
import Coal.Kernel.Language.Op (Op (..))
import qualified Coal.Kernel.Language.Type as NK
import qualified Coal.Kernel.Language.Type.Constructors as NKT
import qualified Coal.Kernel.Language.Type.HasType as NKHT
import Coal.Language
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))

type NKExpr = Expr NK.Type

logicalNotOperator ::
  (Monad m) =>
  (Expression a Kind IndexedType -> CompilerT a m NKExpr) ->
  NonEmpty (Expression a Kind IndexedType) ->
  CompilerT a m NKExpr
logicalNotOperator translate es = do
  args <- traverse translate es
  pure $
    EApp
      t1
      (EVar (Label (NKT.arrow t1 t1) "Builtin$.operator$__not"))
      args
 where
  t1 = translateType (TIntrinsic IBool)

translateOperator ::
  (Monad m, Data a) =>
  (Expression a Kind IndexedType -> CompilerT a m NKExpr) ->
  IndexedType ->
  Operator ->
  NonEmpty (Expression a Kind IndexedType) ->
  CompilerT a m NKExpr
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
      binop translate OAnd (TIntrinsic IBool, TIntrinsic IBool)
    OLogicalOr ->
      binop translate OOr (TIntrinsic IBool, TIntrinsic IBool)
    OStringConcatenation ->
      stringConcatenationOperator translate

stringConcatenationOperator ::
  (Monad m) =>
  (Expression a Kind IndexedType -> CompilerT a m NKExpr) ->
  NonEmpty (Expression a Kind IndexedType) ->
  CompilerT a m NKExpr
stringConcatenationOperator translate es = do
  args <- traverse translate es
  pure $
    EApp
      t1
      (EVar (Label (NKT.arrow t1 (NKT.arrow t1 t1)) "Builtin$.operator$__string_concatenation"))
      args
  where 
    t1 = translateType (TIntrinsic IString)

listConcatenationOperator ::
  (Monad m) =>
  (Expression a Kind IndexedType -> CompilerT a m NKExpr) ->
  IndexedType ->
  NonEmpty (Expression a Kind IndexedType) ->
  CompilerT a m NKExpr
listConcatenationOperator translate t es = do
  args <- traverse translate es
  pure $
    EApp
      t1
      (EVar (Label (NKT.arrow t1 (NKT.arrow t1 t1)) "Builtin$.operator$__list_concatenation"))
      args
  where 
    t1 = translateType t

reverseCompositionOperator ::
  (Monad m) =>
  (Expression a Kind IndexedType -> CompilerT a m NKExpr) ->
  IndexedType ->
  NonEmpty (Expression a Kind IndexedType) ->
  CompilerT a m NKExpr
reverseCompositionOperator translate t es = do
  args <- traverse translate es
  pure $
    EApp
      t1
      (EVar (Label (NKHT.foldType t1 (NKHT.typeOf <$> args)) "Builtin$.operator$__reverse_composition"))
      args
  where
    t1 = translateType t

forwardCompositionOperator ::
  (Monad m) =>
  (Expression a Kind IndexedType -> CompilerT a m NKExpr) ->
  IndexedType ->
  NonEmpty (Expression a Kind IndexedType) ->
  CompilerT a m NKExpr
forwardCompositionOperator translate t es = do
  args <- traverse translate es
  pure $
    EApp
      t1
      (EVar (Label (NKHT.foldType t1 (NKHT.typeOf <$> args)) "Builtin$.operator$__forward_composition"))
      args
  where
    t1 = translateType t

reverseApplicationOperator ::
  (Monad m) =>
  (Expression a Kind IndexedType -> CompilerT a m NKExpr) ->
  IndexedType ->
  NonEmpty (Expression a Kind IndexedType) ->
  CompilerT a m NKExpr
reverseApplicationOperator translate t es = do
  args <- traverse translate es
  pure $
    EApp
      t1
      (EVar (Label (NKHT.foldType t1 (NKHT.typeOf <$> args)) "Builtin$.operator$__reverse_application"))
      args
  where
    t1 = translateType t

forwardApplicationOperator ::
  (Monad m) =>
  (Expression a Kind IndexedType -> CompilerT a m NKExpr) ->
  IndexedType ->
  NonEmpty (Expression a Kind IndexedType) ->
  CompilerT a m NKExpr
forwardApplicationOperator translate t es = do
  args <- traverse translate es
  pure $
    EApp
      t1
      (EVar (Label (NKHT.foldType t1 (NKHT.typeOf <$> args)) "Builtin$.operator$__forward_application"))
      args
  where
    t1 = translateType t

binop ::
  (Monad m, Data a) =>
  (Expression a Kind IndexedType -> CompilerT a m NKExpr) ->
  (NKExpr -> NKExpr -> Op NKExpr) ->
  (IndexedType, IndexedType) ->
  NonEmpty (Expression a Kind IndexedType) ->
  CompilerT a m NKExpr
binop translate op (t1, t2) (e1 :| [e2])
  | e1 `hasType` t1 && e2 `hasType` t2 = do
      o1 <- translate e1
      o2 <- translate e2
      pure (EOp (op o1 o2))
binop _ _ _ _ = error "Implementation error"

{-# INLINE hasType #-}
hasType :: (Data a) => Expression a Kind IndexedType -> IndexedType -> Bool
hasType e t = typeOf e == t
