{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

-- TODO: rename
module Noll.Compiler2.Params where

import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Lang.Common.Environment (Environment (..))
import Lang.Common.List1 (List1, fromList1)
import Lang.Common.Supply (Supply (..), supplied)
import Lang.Utils (Name, traverse_)
import Noll.Language

import qualified Data.Text as Text
import qualified Lang.Common.Environment as Environment

instantiateVars :: (MonadState s m, Supply s) => [(Name, TypeIndex Kind)] -> Environment Kind -> Type Parameter () -> m IndexedType
instantiateVars ts0 env t = do
  ts <- execWriterT (params t)
  runReaderT (instantiateTypeVars t) (Environment.fromList (ts0 <> ts), env)

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

class Params p where
  params :: (MonadState s m, Supply s) => p -> WriterT [(Name, TypeIndex Kind)] m ()

instance (Params a) => Params [a] where
  params = traverse_ params

instance (Params a) => Params (List1 a) where
  params = traverse_ params

instance Params (Type Parameter ()) where
  params =
    \case
      TVariable p ->
        params p
      TApplication _ t ts -> do
        params t
        params ts
      TArrow t1 t2 -> do
        params t1
        params t2
      TIntrinsic t ->
        params t
      TRow r ->
        params r
      TAlias _ _ t ->
        params t
      TConstructor{} ->
        pure ()

instance Params (Intrinsic (Type Parameter ())) where
  params =
    \case
      IList t ->
        params t
      IOption t ->
        params t
      IRecord t ->
        params t
      IResult t ->
        params t
      ITuple ts ->
        params ts
      _ ->
        pure ()

instance Params (Row Parameter () (Type Parameter ())) where
  params =
    \case
      RVariable p ->
        params p
      RExtend _ t r -> do
        params t
        params r
      RNil ->
        pure ()

instance Params (Parameter ()) where
  params p = do
    ti <- supplied (TypeIndex KType)
    tell [(parameterName p, ti)]
