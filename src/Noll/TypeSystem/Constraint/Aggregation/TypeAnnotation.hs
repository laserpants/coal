{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint.Aggregation.TypeAnnotation (instantiateAnnotation) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.RWS (MonadReader, asks, get, put)
import Control.Monad.State (MonadState, StateT, evalStateT, gets, modify)
import qualified Data.Map.Strict as Map
import Noll.Language (
  IndexedType,
  Kind (..),
  OpaqueType,
  Row (..),
  Scheme (..),
  Type (..),
  TypeIndex (..),
  TypeParam (..),
  foldKind,
  kindOf,
  typeIndexesIn,
 )
import qualified Noll.Library.Environment as Environment
import Noll.TypeSystem.Constraint.Aggregation.Internal (AggregationContext (..), TypeAnnotationError (..))
import Noll.Utils (Dictionary, IndexMap, Name, lexOrderRank)

type TypeAnnotationContext = AggregationContext TypeIndex Kind IndexedType

{-# INLINE lookupTypeConstructor #-}
lookupTypeConstructor :: (MonadReader TypeAnnotationContext m) => Name -> m (Maybe Kind)
lookupTypeConstructor name = Environment.lookup name <$> asks aggregationTypeConstructorEnv

instantiateAnnotation :: (MonadState (TypeIndex ()) m, MonadReader TypeAnnotationContext m) => Type TypeParam () -> m (Either TypeAnnotationError (Type TypeIndex Kind))
instantiateAnnotation t = do
  TypeIndex _ n <- get
  runExceptT $ do
    t1 <- translateToIndexed t
    evalStateT (addKinds t1) mempty

type AddKinds m a = StateT (IndexMap Kind) (ExceptT TypeAnnotationError m) a

typeIndex :: (MonadReader TypeAnnotationContext m) => Kind -> Int -> AddKinds m (TypeIndex Kind)
typeIndex k n = do
  map <- get
  case Map.lookup n map of
    Nothing -> do
      modify (Map.insert n k)
      pure (TypeIndex k n)
    Just k1
      | k1 /= k ->
          throwError KindMismatch
    Just{} ->
      pure (TypeIndex k n)

addKinds :: (MonadReader TypeAnnotationContext m) => OpaqueType -> AddKinds m IndexedType
addKinds =
  \case
    TApplication _ (TVariable (TypeIndex _ n)) ts -> do
      ts1 <- traverse addKinds ts
      var <- typeIndex (foldKind KType (kindOf <$> ts1)) n
      pure (TApplication KType (TVariable var) ts1)
    TApplication _ t ts ->
      TApplication KType
        <$> addKinds t
        <*> traverse addKinds ts
    TVariable (TypeIndex _ n) ->
      TVariable <$> typeIndex KType n
    TArrow t1 t2 ->
      TArrow <$> addKinds t1 <*> addKinds t2
    TConstructor _ name -> do
      c <- lookupTypeConstructor name
      case c of
        Nothing ->
          throwError (NoTypeConstructor name)
        Just k ->
          pure (TConstructor k name)
    TIntrinsic t ->
      TIntrinsic <$> traverse addKinds t
    TRow row ->
      TRow <$> addKindsRow row
    TAlias name ts t ->
      TAlias name <$> traverse addKinds ts <*> addKinds t

addKindsRow :: (MonadReader TypeAnnotationContext m) => Row TypeIndex () OpaqueType -> AddKinds m (Row TypeIndex Kind IndexedType)
addKindsRow =
  \case
    RVariable (TypeIndex _ n) ->
      RVariable <$> typeIndex KRow n
    RExtend name t row ->
      RExtend name <$> addKinds t <*> addKindsRow row
    RNil ->
      pure RNil

translateToIndexed :: (MonadState (TypeIndex ()) m) => Type TypeParam () -> m OpaqueType
translateToIndexed =
  \case
    TApplication _ t ts ->
      TApplication () <$> translateToIndexed t <*> traverse translateToIndexed ts
    TArrow t1 t2 ->
      TArrow <$> translateToIndexed t1 <*> translateToIndexed t2
    TConstructor _ name ->
      pure (TConstructor () name)
    TIntrinsic t ->
      TIntrinsic <$> traverse translateToIndexed t
    TRow row ->
      TRow <$> translateToIndexedRow row
    TVariable (TypeParam _ name) ->
      TVariable <$> toTypeIndex name
    TAlias name ts t ->
      TAlias name <$> traverse translateToIndexed ts <*> translateToIndexed t

translateToIndexedRow :: (MonadState (TypeIndex ()) m) => Row TypeParam () (Type TypeParam ()) -> m (Row TypeIndex () OpaqueType)
translateToIndexedRow =
  \case
    RExtend name t row ->
      RExtend name <$> translateToIndexed t <*> translateToIndexedRow row
    RVariable (TypeParam _ name) ->
      RVariable <$> toTypeIndex name
    RNil ->
      pure RNil

toTypeIndex :: (MonadState (TypeIndex ()) m) => Name -> m (TypeIndex ())
toTypeIndex name = do
  TypeIndex _ n <- get
  pure (TypeIndex () (n + lexOrderRank name))
