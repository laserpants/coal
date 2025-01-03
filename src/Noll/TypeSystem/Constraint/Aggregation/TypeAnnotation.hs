{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint.Aggregation.TypeAnnotation where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.RWS (
  MonadRWS,
  MonadReader,
  MonadState,
  MonadWriter,
  RWS,
  asks,
  evalRWS,
  get,
  local,
  put,
 )
import Control.Monad.State (StateT, evalStateT, gets, modify)
import Control.Monad.Trans (lift)
import Data.List (partition)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Noll.Label (Label (..))
import Noll.Language (
  Binding (..),
  Constructor (..),
  Expression (..),
  HasType (..),
  IndexedType,
  Intrinsic (..),
  Kind (..),
  OpaqueType,
  Pattern (..),
  Row (..),
  Scheme (..),
  Type (..),
  TypeIndex (..),
  TypeIndexed (..),
  TypeParam (..),
  foldKind,
  foldType,
  kindOf,
  typeIndexesIn,
 )
import Noll.Library.Environment (Environment (..))
import qualified Noll.Library.Environment as Environment
import Noll.Library.List1 (List1, NonEmpty ((:|)), fromList1)
import qualified Noll.Library.List1 as List1
import Noll.TypeSystem.Constraint (Constraint (..), MonomorphicSet (..), overMonomorphicSet)
import Noll.TypeSystem.Constraint.Rule (Assumption (..), InferenceRule (..), assumptionNameIs)
import Noll.Utils (Dictionary, IndexMap, Name, concatMapM, forM, tellLeft, tellRight)
import Noll.TypeSystem.Constraint.Aggregation.Internal

type TypeAnnotationContext = AggregationContext TypeIndex Kind IndexedType

{-# INLINE lookupTypeConstructor #-}
lookupTypeConstructor :: (MonadReader TypeAnnotationContext m) => Name -> m (Maybe Kind)
lookupTypeConstructor name = Environment.lookup name <$> asks aggregationTypeConstructorEnv

instantiateAnnotation :: (MonadReader TypeAnnotationContext m) => Type TypeParam () -> m (Either TypeAnnotationError (Scheme TypeIndex Kind IndexedType))
instantiateAnnotation t = do
  r <- runExceptT $ do
    t1 <- evalStateT (translateToIndexed t) (0, mempty)
    evalStateT (addKinds t1) mempty
  pure (scheme <$> r)
 where
  scheme t = Forall (typeIndexesIn t) [] t

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
          throwError (TypeConstructorMissing name)
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

type Instantiate m = StateT (Int, Dictionary OpaqueType) (ExceptT TypeAnnotationError m) 

translateToIndexed :: (Monad m) => Type TypeParam () -> Instantiate m OpaqueType
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
    TVariable (TypeParam _ name) -> do
      dict <- gets snd
      case Map.lookup name dict of
        Nothing ->
          freshVariable name id TVariable
        Just t@TVariable{} ->
          pure t
        Just _ ->
          throwError KindMismatch
    TAlias name ts t ->
      TAlias name <$> traverse translateToIndexed ts <*> translateToIndexed t

translateToIndexedRow :: (Monad m) => Row TypeParam () (Type TypeParam ()) -> Instantiate m (Row TypeIndex () OpaqueType)
translateToIndexedRow =
  \case
    RExtend name t row ->
      RExtend name
        <$> translateToIndexed t
        <*> translateToIndexedRow row
    RVariable (TypeParam _ name) -> do
      dict <- gets snd
      case Map.lookup name dict of
        Nothing ->
          freshVariable name TRow RVariable
        Just (TRow row) ->
          pure row
        Just _ ->
          throwError KindMismatch
    RNil ->
      pure RNil

freshVariable :: (Monad m) => Name -> (t -> OpaqueType) -> (TypeIndex () -> t) -> Instantiate m t
freshVariable name from to = do
  (n, dict) <- get
  let t = to (TypeIndex () n)
  put (succ n, Map.insert name (from t) dict)
  pure t
