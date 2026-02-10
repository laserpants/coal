{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Coal.ProtoTypeSystem.Annotations where

import Coal.TypeSystem.Constraint.Generation.Context
import Coal.ProtoCompiler.ProtoBuild.ProtoNameEntry
import Coal.Language
import Coal.TypeSystem.Constraint.Generation.Annotation.Error (TypeAnnotationError (..))
import Control.Monad.Except (ExceptT, MonadError, runExceptT, withExceptT, throwError)
import Control.Monad.State (MonadState, StateT, get, runStateT)
import Control.Monad.Reader (asks)
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Dictionary, Name)
import qualified Coal.Common.Environment as Environment

type TypeIndexMap = Dictionary (TypeIndex Kind)

newtype AnnotationsT m a o = AnnotationsT
  {annotationsMonad :: ExceptT (a -> TypeAnnotationError a) (StateT TypeIndexMap m) o}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState TypeIndexMap
    , MonadError (a -> TypeAnnotationError a)
    )

runAnnotationsT :: (Monad m) => a -> AnnotationsT m a t -> m (Either (TypeAnnotationError a) t, TypeIndexMap)
runAnnotationsT loc o = runStateT (runExceptT (withExceptT ($ loc) (annotationsMonad o))) mempty

lookupTypeConstructor :: (Monad m) => Name -> AnnotationsT m a (Maybe Kind)
lookupTypeConstructor name = do
  env <- undefined -- asks constraintsGenContextTypeConstructors
  case Environment.lookup name env of
    Nothing ->
      pure Nothing
    Just ProtoTypeConstructorEntry{..} ->
      pure (Just protoOtypeConstructorEntryKind)

indexTypeAnnotations :: (Monad m) => Type Parameter Kind -> AnnotationsT m a IndexedType
indexTypeAnnotations =
  \case
    TApplication{} -> do
      undefined
    TVariable (Parameter k v) ->
      TVariable <$> foo k v
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

indexTypeAnnotationsInRow :: (Monad m) => Row Parameter Kind (Type Parameter Kind) -> AnnotationsT m a (Row TypeIndex Kind IndexedType)
indexTypeAnnotationsInRow =
  \case
    RVariable (Parameter k v) ->
      RVariable <$> foo k v
    RExtend name t row ->
      RExtend name <$> indexTypeAnnotations t <*> indexTypeAnnotationsInRow row
    RNil ->
      pure RNil

indexTypeApplicationTypeAnnotations :: (Monad m) => Type Parameter Kind -> NonEmpty (Type Parameter Kind) -> AnnotationsT m a IndexedType
indexTypeApplicationTypeAnnotations con@(TConstructor _ name) ts  
  | isTupleType con =
      undefined
indexTypeApplicationTypeAnnotations (TVariable (Parameter _ v)) ts = 
  undefined
indexTypeApplicationTypeAnnotations t ts = 
  undefined

foo :: (Monad m) => Kind -> Name -> AnnotationsT m a (TypeIndex Kind)
foo kind name = do
  dict <- get
  undefined
