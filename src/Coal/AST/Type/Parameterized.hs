{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.AST.Type.Parameterized (
  instantiateVars,
  instantiateTypeVars,
  instantiateTypeIndexes,
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Common.Supply (Supply (..), supplied)
import Coal.Language
import Coal.ProtoTypeSystem.Kind.Error (ProtoKindError (..))
import Control.Monad.Except
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.List.NonEmpty (NonEmpty, toList)
import Extras (Name, traverse_)

instantiateVars :: (MonadState s m, Supply s) => [(Name, TypeIndex Kind)] -> Environment Kind -> Type Parameter k -> m (Either ProtoKindError IndexedType)
instantiateVars ts0 env t = do
  ts1 <- execWriterT (instantiateTypeIndexes t)
  runReaderT (runExceptT (instantiateTypeVars t)) (Environment.fromList (ts0 <> ts1), env)

instantiateTypeApplication :: (MonadState s m, Supply s) => Type Parameter k -> NonEmpty (Type Parameter k) -> ExceptT ProtoKindError (ReaderT (Environment (TypeIndex Kind), Environment Kind) m) IndexedType
instantiateTypeApplication con@(TConstructor _ name) ts
  | isTupleType con =
      applyTypeArgs KType (TConstructor (tupleKind (length ts)) name)
        <$> traverse instantiateTypeVars ts
instantiateTypeApplication t ts = do
  u <- instantiateTypeVars t
  us <- traverse instantiateTypeVars ts
  case applyKind (kindOf <$> toList us) (kindOf u) of
    Nothing ->
      throwError ProtoECannotUnifyKinds
    Just k ->
      pure (applyTypeArgs k u us)

instantiateTypeVars :: (MonadState s m, Supply s) => Type Parameter k -> ExceptT ProtoKindError (ReaderT (Environment (TypeIndex Kind), Environment Kind) m) IndexedType
instantiateTypeVars =
  \case
    TVariable (Parameter _ n) -> do
      env <- asks fst
      case Environment.lookup n env of
        Just v ->
          pure (TVariable v)
        Nothing ->
          error "Implementation error"
    t@TApplication{} ->
      uncurry instantiateTypeApplication (listTypeArgs t)
    TArrow t1 t2 ->
      TArrow <$> instantiateTypeVars t1 <*> instantiateTypeVars t2
    TIntrinsic t ->
      pure (TIntrinsic t)
    TRecord t ->
      TRecord <$> instantiateTypeVars t
    TRow r ->
      TRow <$> instantiateRowVars r
    TAlias name ts t ->
      TAlias name <$> traverse instantiateTypeVars ts <*> instantiateTypeVars t
    TConstructor _ "List" ->
      pure (TConstructor (KArrow KType KType) "List")
    TConstructor _ name -> do
      env <- asks snd
      case Environment.lookup name env of
        Just k ->
          pure (TConstructor k name)
        Nothing ->
          throwError (ProtoENoTypeConstructor name)

instantiateRowVars :: (MonadState s m, Supply s) => Row Parameter k (Type Parameter k) -> ExceptT ProtoKindError (ReaderT (Environment (TypeIndex Kind), Environment Kind) m) (Row TypeIndex Kind IndexedType)
instantiateRowVars =
  \case
    RVariable (Parameter _ n) -> do
      env <- asks fst
      case Environment.lookup n env of
        Just v ->
          pure (RVariable v)
        Nothing ->
          error "Implementation error"
    RExtend name t r ->
      RExtend name <$> instantiateTypeVars t <*> instantiateRowVars r
    RNil ->
      pure RNil

class Parameterized p where
  instantiateTypeIndexes :: (MonadState s m, Supply s) => p -> WriterT [(Name, TypeIndex Kind)] m ()

instance (Parameterized p) => Parameterized [p] where
  instantiateTypeIndexes = traverse_ instantiateTypeIndexes

instance (Parameterized p) => Parameterized (NonEmpty p) where
  instantiateTypeIndexes = traverse_ instantiateTypeIndexes

instance Parameterized (Type Parameter k) where
  instantiateTypeIndexes =
    \case
      TVariable p ->
        instantiateTypeIndexes p
      TApplication _ t1 t2 -> do
        instantiateTypeIndexes t1
        instantiateTypeIndexes t2
      TArrow t1 t2 -> do
        instantiateTypeIndexes t1
        instantiateTypeIndexes t2
      TRecord t ->
        instantiateTypeIndexes t
      TRow r ->
        instantiateTypeIndexes r
      TAlias _ _ t ->
        instantiateTypeIndexes t
      TConstructor{} ->
        pure ()
      TIntrinsic{} ->
        pure ()

instance Parameterized (Row Parameter k (Type Parameter k)) where
  instantiateTypeIndexes =
    \case
      RVariable p ->
        instantiateTypeIndexes p
      RExtend _ t r -> do
        instantiateTypeIndexes t
        instantiateTypeIndexes r
      RNil ->
        pure ()

instance Parameterized (Parameter k) where
  instantiateTypeIndexes p = do
    ti <- supplied (TypeIndex KType)
    tell [(parameterName p, ti)]
