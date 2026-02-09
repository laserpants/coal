{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Coal.ProtoTypeSystem.Annotations where

import Coal.Language
import Coal.TypeSystem.Constraint.Generation.Annotation.Error (TypeAnnotationError (..))
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.State (MonadState, StateT, get)
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Dictionary, Name)

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
runAnnotationsT = undefined

indexTypeApplicationTypeAnnotations :: (Monad m) => Type Parameter Kind -> NonEmpty (Type Parameter Kind) -> AnnotationsT m a IndexedType
indexTypeApplicationTypeAnnotations = undefined

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
      undefined
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

foo :: (Monad m) => Kind -> Name -> AnnotationsT m a (TypeIndex Kind)
foo kind name = do
  dict <- get
  undefined
