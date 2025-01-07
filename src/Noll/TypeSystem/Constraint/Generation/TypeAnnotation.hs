{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint.Generation.TypeAnnotation (
  TypeAnnotationError (..),
  instantiateAnnotation,
  checkTypeAnnotationParameters,
  runTypeAnnotation,
) where

import Control.Monad.Except (ExceptT, runExceptT, throwError, withExceptT)
import Control.Monad.RWS (MonadReader, asks, get)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT, gets, modify, runStateT)
import Control.Monad.Writer (MonadWriter, tell)
import Data.List.Extra (groupSortOn)
import Debug.Trace
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
import Noll.TypeSystem.Constraint.Generation.Internal (
  ConstraintsGenerationContext (..),
  TypeAnnotationError (..),
 )
import Noll.TypeSystem.Substitution (Substitution (..))
import Noll.Utils (
  Dictionary,
  Name,
  concatMapM,
  forM_,
  lexOrderRank,
  (<$$>),
 )

import qualified Data.Map.Strict as Map
import qualified Noll.Lib.Environment as Environment

type TypeAnnotationContext = ConstraintsGenerationContext TypeIndex Kind IndexedType

{-# INLINE lookupTypeConstructor #-}
lookupTypeConstructor :: (MonadReader TypeAnnotationContext m) => Name -> m (Maybe Kind)
lookupTypeConstructor name = Environment.lookup name <$> asks constraintsGenerationTypeConstructorEnv

instantiateAnnotation ::
  (MonadReader TypeAnnotationContext m, MonadState (Dictionary (a, TypeIndex Kind)) m) =>
  a ->
  Type TypeParam () ->
  m (Either (TypeAnnotationError a) (Type TypeIndex Kind))
instantiateAnnotation loc t = do
  (t, s) <- runTypeAnnotation loc (instantiate t)
  forM_ (Map.toList s) $ \(n, k) -> modify (Map.insert n (loc, k))
  return t

type TypeAnnotation a m = ExceptT (a -> TypeAnnotationError a) (StateT (Dictionary (TypeIndex Kind)) m)

runTypeAnnotation :: (Monad m) => a -> TypeAnnotation a m t -> m (Either (TypeAnnotationError a) t, Dictionary (TypeIndex Kind))
runTypeAnnotation loc v = runStateT (runExceptT (withExceptT ($ loc) v)) mempty

instantiate :: (MonadReader TypeAnnotationContext m) => Type TypeParam () -> TypeAnnotation a m IndexedType
instantiate =
  \case
    TApplication _ (TVariable (TypeParam _ v)) ts -> do
      ts1 <- traverse instantiate ts
      let k = foldKind KType (kindOf <$> ts1)
      t1 <- TVariable <$> typeIndex k v
      pure (TApplication KType t1 ts1)
    TApplication _ t ts ->
      TApplication KType <$> instantiate t <*> traverse instantiate ts
    TVariable (TypeParam _ v) ->
      TVariable <$> typeIndex KType v
    TArrow t1 t2 ->
      TArrow <$> instantiate t1 <*> instantiate t2
    TConstructor _ name -> do
      c <- lookupTypeConstructor name
      case c of
        Nothing ->
          throwError (`NoTypeConstructor` name)
        Just k ->
          pure (TConstructor k name)
    TIntrinsic t ->
      TIntrinsic <$> traverse instantiate t
    TRow row ->
      TRow <$> instantiateRow row
    TAlias name ts t ->
      TAlias name <$> traverse instantiate ts <*> instantiate t

instantiateRow :: (MonadReader TypeAnnotationContext m) => Row TypeParam () (Type TypeParam ()) -> TypeAnnotation a m (Row TypeIndex Kind IndexedType)
instantiateRow =
  \case
    RVariable (TypeParam _ v) ->
      RVariable <$> typeIndex KRow v
    RExtend name t row ->
      RExtend name <$> instantiate t <*> instantiateRow row
    RNil ->
      pure RNil

typeIndex :: (MonadReader TypeAnnotationContext m) => Kind -> Name -> TypeAnnotation a m (TypeIndex Kind)
typeIndex k name = do
  n <- asks constraintsGenerationIndexTreshold
  let index = TypeIndex k (n + lexOrderRank name)
  dict <- get
  case Map.lookup name dict of
    Nothing -> do
      modify (Map.insert name index)
      pure index
    Just (TypeIndex k1 _)
      | k1 /= k ->
          throwError KindMismatch
    Just{} ->
      pure index

checkTypeAnnotationParameters :: (MonadWriter [TypeAnnotationError a] m) => [(Name, (a, TypeIndex Kind))] -> Substitution -> m ()
checkTypeAnnotationParameters ps (Substitution sub) = do
  params <- groupSortOn fst <$> concatMapM go ps
  let twoOreMore xs = length xs > 1
  case filter twoOreMore params of
    [] ->
      pure ()
    qs ->
      tell [NonDistinctTypeParameters (snd <$$> qs)]
 where
  go (name, (loc, TypeIndex _ index)) =
    case Map.lookup index sub of
      Just (TVariable (TypeIndex _ n)) ->
        pure [(n, (name, loc))]
      Just t -> do
        tell [ResolvesToMonomorphicType name t]
        pure []
      Nothing ->
        pure [(index, (name, loc))]
