{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Language.Type.Kind.Indexed (ToKindIndexed (..)) where

import Coal.Common.Supply (supply)
import Coal.Language.Type (Parameter (..), Type (..), TypeIndex (..))
import Coal.Language.Type.Kind (Kind (..))
import Coal.Language.Type.Row (Row (..))
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule
import Control.Monad.State (State)

class ToKindIndexed a b where
  toKindIndexed :: a -> State Int b

instance (ToKindIndexed a b) => ToKindIndexed [a] [b] where
  toKindIndexed = traverse toKindIndexed

instance ToKindIndexed (ProtoDefinition a k t) (ProtoDefinition a Kind t) where
  toKindIndexed =
    \case
      ProtoDType loc name def ->
        undefined
      ProtoDTypeAlias loc name ->
        undefined
      ProtoDFunction loc name def ->
        undefined
      ProtoDFunctionGroup loc name ->
        undefined
      ProtoDFold loc name ->
        undefined
      ProtoDLet loc name def ->
        undefined
      ProtoDImport loc path imports ->
        undefined
      ProtoDQualifiedImport loc path ->
        undefined
      ProtoDTrait loc name def ->
        undefined
      ProtoDInstance loc def ->
        undefined

instance ToKindIndexed (ProtoModule a k t) (ProtoModule a Kind t) where
  toKindIndexed =
    \case
      ProtoModule{..} -> do
        newProtoOmoduleDefinitions <- traverse toKindIndexed protoOmoduleDefinitions
        pure ProtoModule{protoOmoduleDefinitions = newProtoOmoduleDefinitions, ..}

instance ToKindIndexed (TypeIndex k) (TypeIndex Kind) where
  toKindIndexed =
    \case
      TypeIndex{..} ->
        TypeIndex <$> kVar <*> pure typeIndexId

instance ToKindIndexed (Parameter k) (Parameter Kind) where
  toKindIndexed =
    \case
      Parameter{..} ->
        Parameter <$> kVar <*> pure parameterName

instance (ToKindIndexed (o k) (o Kind)) => ToKindIndexed (Type o k) (Type o Kind) where
  toKindIndexed =
    \case
      TApplication _ t1 t2 -> do
        TApplication <$> kVar <*> toKindIndexed t1 <*> toKindIndexed t2
      TArrow t1 t2 ->
        TArrow <$> toKindIndexed t1 <*> toKindIndexed t2
      TConstructor _ name ->
        TConstructor <$> kVar <*> pure name
      TIntrinsic t ->
        pure (TIntrinsic t)
      TRecord t ->
        TRecord <$> toKindIndexed t
      TRow r ->
        TRow <$> toKindIndexed r
      TVariable v ->
        TVariable <$> toKindIndexed v
      TAlias name ts t ->
        TAlias name <$> traverse toKindIndexed ts <*> toKindIndexed t

instance (ToKindIndexed (o k) (o Kind)) => ToKindIndexed (Row o k (Type o k)) (Row o Kind (Type o Kind)) where
  toKindIndexed =
    \case
      RExtend name t r ->
        RExtend name <$> toKindIndexed t <*> toKindIndexed r
      RVariable v ->
        RVariable <$> toKindIndexed v
      RNil ->
        pure RNil

kVar :: State Int Kind
kVar = KVar <$> supply
