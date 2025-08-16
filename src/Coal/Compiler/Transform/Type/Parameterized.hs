{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Transform.Type.Parameterized where

import Coal.Common.Environment (Environment (..))
import Coal.Common.List1 (List1, fromList1)
import Coal.Common.Supply (Supply (..), supplied)
import Coal.Language
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Extra (Name, traverse_)

import qualified Coal.Common.Environment as Environment
import qualified Data.Text as Text

instantiateVars :: (MonadState s m, Supply s) => [(Name, TypeIndex Kind)] -> Environment Kind -> Type Parameter () -> m IndexedType
instantiateVars ts0 env t = do
  ts1 <- execWriterT (instantiateTypeIndexes t)
  runReaderT (instantiateTypeVars t) (Environment.fromList (ts0 <> ts1), env)

instantiateTypeVars :: (MonadState s m, Supply s) => Type Parameter () -> ReaderT (Environment (TypeIndex Kind), Environment Kind) m IndexedType
instantiateTypeVars =
  \case
    TVariable (Parameter _ n) -> do
      env <- asks fst
      case Environment.lookup n env of
        Just v ->
          pure (TVariable v)
        Nothing ->
          error "Implementation error"
    TApplication _ (TConstructor _ name) ts
      | "#Tuple" `Text.isPrefixOf` name ->
          TApplication KType (TConstructor (tupleKind (length ts)) name)
            <$> traverse instantiateTypeVars ts
    TApplication _ t ts -> do
      u <- instantiateTypeVars t
      us <- traverse instantiateTypeVars ts
      case applyKind (kindOf <$> fromList1 us) (kindOf u) of
        Nothing ->
          error "Kind mismatch"
        Just k ->
          pure (TApplication k u us)
    TArrow t1 t2 ->
      TArrow <$> instantiateTypeVars t1 <*> instantiateTypeVars t2
    TIntrinsic t ->
      TIntrinsic <$> traverse instantiateTypeVars t
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
          error ("No type constructor '" <> Text.unpack name <> "'")

instantiateRowVars :: (MonadState s m, Supply s) => Row Parameter () (Type Parameter ()) -> ReaderT (Environment (TypeIndex Kind), Environment Kind) m (Row TypeIndex Kind IndexedType)
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

instance (Parameterized a) => Parameterized [a] where
  instantiateTypeIndexes = traverse_ instantiateTypeIndexes

instance (Parameterized a) => Parameterized (List1 a) where
  instantiateTypeIndexes = traverse_ instantiateTypeIndexes

instance Parameterized (Type Parameter ()) where
  instantiateTypeIndexes =
    \case
      TVariable p ->
        instantiateTypeIndexes p
      TApplication _ t ts -> do
        instantiateTypeIndexes t
        instantiateTypeIndexes ts
      TArrow t1 t2 -> do
        instantiateTypeIndexes t1
        instantiateTypeIndexes t2
      TIntrinsic t ->
        instantiateTypeIndexes t
      TRow r ->
        instantiateTypeIndexes r
      TAlias _ _ t ->
        instantiateTypeIndexes t
      TConstructor{} ->
        pure ()

instance Parameterized (Intrinsic (Type Parameter ())) where
  instantiateTypeIndexes =
    \case
      IRecord t ->
        instantiateTypeIndexes t
      _ ->
        pure ()

instance Parameterized (Row Parameter () (Type Parameter ())) where
  instantiateTypeIndexes =
    \case
      RVariable p ->
        instantiateTypeIndexes p
      RExtend _ t r -> do
        instantiateTypeIndexes t
        instantiateTypeIndexes r
      RNil ->
        pure ()

instance Parameterized (Parameter ()) where
  instantiateTypeIndexes p = do
    ti <- supplied (TypeIndex KType)
    tell [(parameterName p, ti)]
