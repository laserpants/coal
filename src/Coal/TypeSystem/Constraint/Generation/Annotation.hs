{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Coal.TypeSystem.Constraint.Generation.Annotation (
  TypeAnnotationError,
  instantiateAnnotation,
  checkTypeAnnotationParameters,
  runTypeAnnotation,
) where

import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build (ModuleBuild (..), TypeConstructorEntry (..))
import Coal.Language
import Coal.ProtoCompiler.ProtoBuild.ProtoNameEntry
import Coal.TypeSystem.Constraint.Generation.Stack
import Coal.TypeSystem.Constraint.Generation.State (overConstraintsGenStateTypeIndexes)
import Coal.TypeSystem.Substitution (Substitution (..))
import Coal.Utils (lexOrderRank)
import Control.Arrow ((>>>))
import Control.Monad.Except (ExceptT, runExceptT, throwError, withExceptT)
import Control.Monad.RWS (MonadReader, asks, get)
import Control.Monad.State (MonadState, StateT, modify, runStateT)
import Control.Monad.Writer (MonadWriter, tell)
import Data.List.Extra (groupSortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import Extras (Dictionary, Name, concatMapM, forM_)

type TypeAnnotationContext a = ConstraintsGenContext a TypeIndex Kind IndexedType

lookupTypeConstructor :: (MonadReader (TypeAnnotationContext a) m) => Name -> m (Maybe Kind)
lookupTypeConstructor name = do
  env <- asks constraintsGenContextTypeConstructors
  case Environment.lookup name env of
    Nothing ->
      pure Nothing
    Just (ProtoTypeConstructorEntry _ _ kind _) ->
      pure (Just kind)

instantiateAnnotation ::
  (MonadReader (TypeAnnotationContext a) m, MonadState (ConstraintsGenState a) m) =>
  a ->
  Type Parameter () ->
  m (Either (TypeAnnotationError a) (Type TypeIndex Kind))
instantiateAnnotation loc a = do
  (t, s) <- runTypeAnnotation loc (instantiate a)
  forM_ (Map.toList s) $
    \(n, k) ->
      modify (overConstraintsGenStateTypeIndexes (Map.insert n (loc, k)))
  return t

type TypeAnnotation a m = ExceptT (a -> TypeAnnotationError a) (StateT (Dictionary (TypeIndex Kind)) m)

runTypeAnnotation :: (Monad m) => a -> TypeAnnotation a m t -> m (Either (TypeAnnotationError a) t, Dictionary (TypeIndex Kind))
runTypeAnnotation loc v = runStateT (runExceptT (withExceptT ($ loc) v)) mempty

instantiateTypeApplication :: (MonadReader (TypeAnnotationContext a) m) => Type Parameter () -> NonEmpty (Type Parameter ()) -> TypeAnnotation a m IndexedType
instantiateTypeApplication con@(TConstructor _ name) ts
  | isTupleType con =
      applyTypeArgs KType (TConstructor (tupleKind (length ts)) name)
        <$> traverse instantiate ts
instantiateTypeApplication (TVariable (Parameter _ v)) ts = do
  ts' <- traverse instantiate ts
  t' <- TVariable <$> typeIndex (foldKind KType (kindOf <$> ts')) v
  pure (applyTypeArgs KType t' ts')
instantiateTypeApplication t ts =
  applyTypeArgs KType <$> instantiate t <*> traverse instantiate ts

instantiate :: (MonadReader (TypeAnnotationContext a) m) => Type Parameter () -> TypeAnnotation a m IndexedType
instantiate =
  \case
    t@TApplication{} -> do
      uncurry instantiateTypeApplication (listTypeArgs t)
    TVariable (Parameter _ v) ->
      TVariable <$> typeIndex KType v
    TArrow t1 t2 ->
      TArrow <$> instantiate t1 <*> instantiate t2
    TConstructor _ name -> do
      c <- lookupTypeConstructor name
      case c of
        Nothing ->
          throwError (`EAnnotationConstructor` name)
        Just k ->
          pure (TConstructor k name)
    TIntrinsic t ->
      pure (TIntrinsic t)
    TRecord t ->
      TRecord <$> instantiate t
    TRow row ->
      TRow <$> instantiateRow row
    TAlias name ts t ->
      TAlias name <$> traverse instantiate ts <*> instantiate t

instantiateRow :: (MonadReader (TypeAnnotationContext a) m) => Row Parameter () (Type Parameter ()) -> TypeAnnotation a m (Row TypeIndex Kind IndexedType)
instantiateRow =
  \case
    RVariable (Parameter _ v) ->
      RVariable <$> typeIndex KRow v
    RExtend name t row ->
      RExtend name <$> instantiate t <*> instantiateRow row
    RNil ->
      pure RNil

typeIndex :: (MonadReader (TypeAnnotationContext a) m) => Kind -> Name -> TypeAnnotation a m (TypeIndex Kind)
typeIndex k name = do
  dict <- get
  let index = TypeIndex k (negate (lexOrderRank name) - 1)
  case Map.lookup name dict of
    Nothing -> do
      modify (Map.insert name index)
      pure index
    Just (TypeIndex k1 _)
      | k1 /= k ->
          throwError EAnnotationKindMismatch
    Just{} ->
      pure index

checkTypeAnnotationParameters :: (MonadWriter [TypeAnnotationError a] m) => [(Name, (a, TypeIndex Kind))] -> Substitution -> m ()
checkTypeAnnotationParameters ps (Substitution sub) = do
  params <- groupSortOn fst <$> concatMapM go ps
  case filter (lengthMoreThan 1) params of
    [] ->
      pure ()
    qs -> do
      tell [EAnnotationNonDistinctParameter loc p | (_, (p, loc)) <- concat qs]
 where
  lengthMoreThan n = length >>> (> n)
  go (name, (loc, TypeIndex _ index)) =
    case Map.lookup index sub of
      Just (TVariable (TypeIndex _ n)) ->
        pure [(n, (name, loc))]
      Just (TRow (RVariable (TypeIndex _ n))) ->
        pure [(n, (name, loc))]
      Just t -> do
        tell [EAnnotationMonomorphicType loc name t]
        pure []
      Nothing ->
        pure [(index, (name, loc))]
