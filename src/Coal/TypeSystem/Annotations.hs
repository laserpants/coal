{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.TypeSystem.Annotations where

import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build.NameEntry
import Coal.Language
import Coal.TypeSystem.Constraint.Generation.Annotation.Error (TypeAnnotationError (..))
import Coal.TypeSystem.Constraint.Generation.Context
import Coal.Utils (lexOrderRank)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError, withExceptT)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State (MonadState, StateT, get, modify, runStateT)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Extras (Dictionary, Name)

type TypeIndexMap = Dictionary (TypeIndex Kind)

type TypeAnnotationContext a = ConstraintsGenContext a TypeIndex Kind IndexedType

newtype AnnotationsT m a o = AnnotationsT
  {annotationsMonad :: ExceptT (a -> TypeAnnotationError a) (ReaderT (TypeAnnotationContext a) (StateT TypeIndexMap m)) o}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState TypeIndexMap
    , MonadError (a -> TypeAnnotationError a)
    , MonadReader (TypeAnnotationContext a)
    )

runAnnotationsT :: (Monad m) => a -> TypeAnnotationContext a -> AnnotationsT m a t -> m (Either (TypeAnnotationError a) t, TypeIndexMap)
runAnnotationsT loc env o = runStateT (runReaderT (runExceptT (withExceptT ($ loc) (annotationsMonad o))) env) mempty

lookupTypeConstructor :: (Monad m) => Name -> AnnotationsT m a (Maybe Kind)
lookupTypeConstructor name = asks (Environment.lookup name . constraintsGenContextTypeConstructors)

--  case Environment.lookup name env of
--    Nothing ->
--      pure Nothing
--    Just TypeConstructorEntry{..} ->
--      pure (Just typeConstructorEntryKind)

indexTypeAnnotations :: (Show a, Monad m) => Type Parameter Kind -> AnnotationsT m a IndexedType
indexTypeAnnotations =
  \case
    t@TApplication{} -> do
      uncurry indexTypeApplicationTypeAnnotations (listTypeArgs t)
    TVariable (Parameter k v) ->
      TVariable <$> typeIndex k v
    TArrow t1 t2 ->
      TArrow <$> indexTypeAnnotations t1 <*> indexTypeAnnotations t2
    TConstructor _ name -> do
      maybeKind <- lookupTypeConstructor name
      case maybeKind of
        Nothing ->
          throwError (`EAnnotationConstructor` name)
        Just kind ->
          pure (TConstructor kind name)
    TIntrinsic t ->
      pure (TIntrinsic t)
    TRecord t ->
      TRecord <$> indexTypeAnnotations t
    TRow row ->
      TRow <$> indexTypeAnnotationsInRow row
    TAlias name ts t ->
      TAlias name <$> traverse indexTypeAnnotations ts <*> indexTypeAnnotations t

indexTypeAnnotationsInRow :: (Show a, Monad m) => Row Parameter Kind (Type Parameter Kind) -> AnnotationsT m a (Row TypeIndex Kind IndexedType)
indexTypeAnnotationsInRow =
  \case
    RVariable (Parameter k v) ->
      RVariable <$> typeIndex k v
    RExtend name t row ->
      RExtend name <$> indexTypeAnnotations t <*> indexTypeAnnotationsInRow row
    RNil ->
      pure RNil

indexTypeApplicationTypeAnnotations :: (Show a, Monad m) => Type Parameter Kind -> NonEmpty (Type Parameter Kind) -> AnnotationsT m a IndexedType
indexTypeApplicationTypeAnnotations con@(TConstructor _ name) ts
  | isTupleType con =
      applyTypeArgs KType (TConstructor (tupleKind (length ts)) name)
        <$> traverse indexTypeAnnotations ts
indexTypeApplicationTypeAnnotations (TVariable (Parameter _ v)) ts = do
  us <- traverse indexTypeAnnotations ts
  u <- TVariable <$> typeIndex (foldKind KType (kindOf <$> us)) v
  pure (applyTypeArgs KType u us)
indexTypeApplicationTypeAnnotations t ts =
  applyTypeArgs KType <$> indexTypeAnnotations t <*> traverse indexTypeAnnotations ts

typeIndex :: (Monad m) => Kind -> Name -> AnnotationsT m a (TypeIndex Kind)
typeIndex kind name = do
  dict <- get
  case Map.lookup name dict of
    Just (TypeIndex k1 _)
      | k1 /= kind ->
          throwError EAnnotationKindMismatch
    Just{} ->
      pure index
    Nothing -> do
      modify (Map.insert name index)
      pure index
 where
  index = TypeIndex kind annotationIndex
  annotationIndex = negate (lexOrderRank name) - 1
