{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.KindConstraint.Collect (
  collectKindConstraints,
  runCollectKindConstraints,
) where

import Control.Monad.RWS (MonadRWS, MonadReader, MonadState, MonadWriter, RWS, ask, evalRWS, runRWS, tell)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Debug.Trace
import Noll.Label (Label (..))
import Noll.Language (
  Binding (..),
  Choice (..),
  Clause (..),
  Expression (..),
  Guard (..),
  HasKind (..),
  IndexedType,
  Intrinsic (..),
  Kind (..),
  KindIndex (..),
  OpaqueRow,
  OpaqueType,
  Pattern (..),
  Row (..),
  Type (..),
  TypeIndex (..),
  foldKind,
  typeOf,
 )
import Noll.Library.Environment (Environment (..))
import qualified Noll.Library.Environment as Environment
import Noll.Library.Supply (supply)
import Noll.TypeSystem.KindConstraint (KindConstraint (..), KindConstraintMetadata (..))
import Noll.Utils (Dictionary, forM_, traverse_)

type KindConstraintsMonad c k = RWS (Environment k) [KindConstraint c k] Int

newtype KindConstraints c k a = KindConstraints {constraintsMonad :: KindConstraintsMonad c k a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (Environment k)
    , MonadWriter [KindConstraint c k]
    , MonadState Int
    , MonadRWS (Environment k) [KindConstraint c k] Int
    )

type CollectConstraints = KindConstraints KindConstraintMetadata (Kind KindIndex)

{-# INLINE runCollectKindConstraints #-}
runCollectKindConstraints :: Environment k -> Int -> KindConstraints c k a -> (a, [KindConstraint c k])
runCollectKindConstraints env n cs = evalRWS (constraintsMonad cs) env n

-- TODO: rename
class TranslateKinds o n | o -> n where
  collectKindConstraints :: o -> CollectConstraints n

instance TranslateKinds (Binding Expression a OpaqueType) (Binding Expression a IndexedType) where
  collectKindConstraints =
    \case
      BPattern a p e ->
        BPattern a <$> collectKindConstraints p <*> collectKindConstraints e

instance TranslateKinds (Pattern a OpaqueType) (Pattern a IndexedType) where
  collectKindConstraints =
    \case
      PVariable a (Label t name) -> do
        t1 <- valueType t
        pure (PVariable a (Label t1 name))
      PConstructor a (Label t name) ps -> do
        t1 <- valueType t
        PConstructor a (Label t1 name)
          <$> traverse collectKindConstraints ps

instance TranslateKinds (Guard Expression a OpaqueType) (Guard Expression a IndexedType) where
  collectKindConstraints =
    \case
      CGuard e ->
        CGuard <$> collectKindConstraints e

instance TranslateKinds (Choice Expression a OpaqueType) (Choice Expression a IndexedType) where
  collectKindConstraints =
    \case
      CPlain a gs e ->
        CPlain a <$> traverse collectKindConstraints gs <*> collectKindConstraints e

instance TranslateKinds (Clause Expression a OpaqueType) (Clause Expression a IndexedType) where
  collectKindConstraints =
    \case
      EClause a p cs ->
        EClause a
          <$> collectKindConstraints p
          <*> traverse collectKindConstraints cs

instance TranslateKinds (Expression a OpaqueType) (Expression a IndexedType) where
  collectKindConstraints =
    \case
      EAnnotation a e ->
        EAnnotation a <$> collectKindConstraints e
      ELiteral a prim ->
        pure (ELiteral a prim)
      EConstructor a (Label t name) -> do
        t1 <- valueType t
        pure (EConstructor a (Label t1 name))
      EVariable a (Label t name) -> do
        t1 <- valueType t
        pure (EVariable a (Label t1 name))
      EApplication a t e es -> do
        t1 <- valueType t
        EApplication a t1
          <$> collectKindConstraints e
          <*> traverse collectKindConstraints es
      EIf a t e1 e2 e3 -> do
        t1 <- valueType t
        EIf a t1
          <$> collectKindConstraints e1
          <*> collectKindConstraints e2
          <*> collectKindConstraints e3
      ELet a gs e1 ->
        ELet a
          <$> traverse collectKindConstraints gs
          <*> collectKindConstraints e1
      ERecursiveLet a p e1 e2 ->
        ERecursiveLet a
          <$> collectKindConstraints p
          <*> collectKindConstraints e1
          <*> collectKindConstraints e2
      ELambda a ps e ->
        ELambda a
          <$> traverse collectKindConstraints ps
          <*> collectKindConstraints e
      EBinaryOperator a (t, op) -> do
        t1 <- valueType t
        pure (EBinaryOperator a (t1, op))
      EUnaryOperator a (t, op) -> do
        t1 <- valueType t
        pure (EUnaryOperator a (t1, op))
      ERecord _ t _ _ ->
        error "TODO"
      EListCons a t e1 e2 -> do
        t1 <- valueType t
        EListCons a t1
          <$> collectKindConstraints e1
          <*> collectKindConstraints e2
      EListLiteral a t es -> do
        t1 <- valueType t
        EListLiteral a t1 <$> traverse collectKindConstraints es
      EMatch a t e cs -> do
        t1 <- valueType t
        EMatch a t1
          <$> collectKindConstraints e
          <*> traverse collectKindConstraints cs

instance TranslateKinds OpaqueType IndexedType where
  collectKindConstraints =
    \case
      TAlias name ts t ->
        TAlias name <$> traverse collectKindConstraints ts <*> collectKindConstraints t
      TApplication _ t ts -> do
        k <- freshKind
        t1 <- collectKindConstraints t
        ts1 <- traverse collectKindConstraints ts
        tell [KindEquality KindConstraintMetadata (kindOf t1) (foldKind k (kindOf <$> ts1))]
        pure (TApplication k t1 ts1)
      TArrow t1 t2 ->
        TArrow <$> collectKindConstraints t1 <*> collectKindConstraints t2
      TIntrinsic t ->
        TIntrinsic <$> traverse collectKindConstraints t
      TRow row ->
        TRow <$> collectKindConstraints row
      TVariable (TypeIndex _ index) ->
        pure (TVariable (TypeIndex (KVariable (KindIndex index)) index))
      TConstructor _ name -> do
        env <- ask
        case Environment.lookup name env of
          Nothing ->
            error "Type not in scope"
          Just k ->
            pure (TConstructor k name)

instance TranslateKinds OpaqueRow (Row TypeIndex (Kind KindIndex) IndexedType) where
  collectKindConstraints =
    \case
      RExtend name t row ->
        RExtend name <$> collectKindConstraints t <*> collectKindConstraints row
      RVariable (TypeIndex _ index) ->
        pure (RVariable (TypeIndex KRow index))
      RNil ->
        pure RNil

valueType :: Type TypeIndex () -> CollectConstraints IndexedType
valueType t = do
  t1 <- collectKindConstraints t
  case kindOf t1 of
    k@KVariable{} -> do
      tell [KindEquality KindConstraintMetadata k KType]
      return t1
    _ ->
      return t1

freshKind :: KindConstraints c (Kind KindIndex) (Kind KindIndex)
freshKind = KVariable . KindIndex <$> supply
