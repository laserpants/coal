{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Language.Type.Kind.Indexed (ToKindIndexed (..)) where

import Coal.Common.Supply (supply)
import Coal.Language.Data.Constructor (DataConstructor (..))
import Coal.Language.Expression (Expression (..))
import Coal.Language.Pattern (Pattern (..))
import Coal.Language.Trait (Trait (..), With (..))
import Coal.Language.Type (Parameter (..), Type (..), TypeIndex (..))
import Coal.Language.Type.Kind (Kind (..))
import Coal.Language.Type.Row (Row (..))
import Coal.Language.Type.Scheme (Scheme (..))
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule
import Control.Monad.State (State)
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra (secondM)

class ToKindIndexed t u where
  toKindIndexed :: t -> State Int u

instance ToKindIndexed () () where
  toKindIndexed _ = pure ()

instance (ToKindIndexed t u) => ToKindIndexed (Maybe t) (Maybe u) where
  toKindIndexed = traverse toKindIndexed

instance (ToKindIndexed t u) => ToKindIndexed [t] [u] where
  toKindIndexed = traverse toKindIndexed

instance (ToKindIndexed t u) => ToKindIndexed (NonEmpty t) (NonEmpty u) where
  toKindIndexed = traverse toKindIndexed

instance (ToKindIndexed t u, Ord u) => ToKindIndexed (Set t) (Set u) where
  toKindIndexed s = Set.fromList <$> toKindIndexed (Set.toList s)

instance (ToKindIndexed t u) => ToKindIndexed (ProtoDefinition a k t) (ProtoDefinition a Kind u) where
  toKindIndexed =
    \case
      ProtoDType loc name def ->
        ProtoDType loc name <$> toKindIndexed def
      ProtoDTypeAlias loc name ->
        pure $ ProtoDTypeAlias loc name
      ProtoDFunction loc name def ->
        ProtoDFunction loc name <$> toKindIndexed def
      ProtoDFunctionGroup loc name ->
        pure $ ProtoDFunctionGroup loc name
      ProtoDFold loc name ->
        pure $ ProtoDFold loc name
      ProtoDLet loc name def ->
        ProtoDLet loc name <$> toKindIndexed def
      ProtoDImport loc path imports ->
        pure $ ProtoDImport loc path imports
      ProtoDQualifiedImport loc path ->
        pure $ ProtoDQualifiedImport loc path
      ProtoDTrait loc name def ->
        ProtoDTrait loc name <$> toKindIndexed def
      ProtoDInstance loc def ->
        ProtoDInstance loc <$> toKindIndexed def

instance ToKindIndexed (ProtoTypeDefinition a k t) (ProtoTypeDefinition a Kind u) where
  toKindIndexed =
    \case
      ProtoTypeDefinition{..} ->
        ProtoTypeDefinition
          <$> toKindIndexed protoOtypeDefinitionParameters
          <*> toKindIndexed protoOtypeDefinitionConstructors

instance (ToKindIndexed t u) => ToKindIndexed (ProtoFunctionDefinition a k t) (ProtoFunctionDefinition a Kind u) where
  toKindIndexed =
    \case
      ProtoFunctionDefinition{..} ->
        ProtoFunctionDefinition protoOfunctionDefinitionMetadata
          <$> toKindIndexed protoOfunctionDefinitionAnnotation
          <*> toKindIndexed protoOfunctionDefinitionType
          <*> toKindIndexed protoOfunctionDefinitionPatterns
          <*> toKindIndexed protoOfunctionDefinitionExpression

instance (ToKindIndexed t u) => ToKindIndexed (ProtoLetDefinition a k t) (ProtoLetDefinition a Kind u) where
  toKindIndexed =
    \case
      ProtoLetDefinition{..} ->
        ProtoLetDefinition protoOletDefinitionMetadata
          <$> toKindIndexed protoOletDefinitionAnnotation
          <*> toKindIndexed protoOletDefinitionType
          <*> toKindIndexed protoOletDefinitionExpression

instance ToKindIndexed (ProtoTraitDefinition a k) (ProtoTraitDefinition a Kind) where
  toKindIndexed =
    \case
      ProtoTraitDefinition{..} ->
        ProtoTraitDefinition protoOtraitDefinitionMetadata
          <$> toKindIndexed protoOtraitDefinitionConstraints
          <*> toKindIndexed protoOtraitDefinitionParameter
          <*> traverse (secondM toKindIndexed) protoOtraitDefinitionInterface

instance (ToKindIndexed t u) => ToKindIndexed (ProtoInstanceDefinition a k t) (ProtoInstanceDefinition a Kind u) where
  toKindIndexed =
    \case
      ProtoInstanceDefinition{..} ->
        ProtoInstanceDefinition protoOinstanceDefinitionMetadata protoOinstanceDefinitionTraitName
          <$> toKindIndexed protoOinstanceDefinitionConstraints
          <*> toKindIndexed protoOinstanceDefinitionType
          <*> toKindIndexed protoOinstanceDefinitionImplementations

instance (ToKindIndexed t u, ToKindIndexed (o k) (o Kind), Ord (o Kind)) => ToKindIndexed (DataConstructor o k t) (DataConstructor o Kind u) where
  toKindIndexed =
    \case
      DataConstructor{..} ->
        DataConstructor constructorName constructorArity <$> toKindIndexed constructorScheme

instance (ToKindIndexed t u) => ToKindIndexed (Trait t) (Trait u) where
  toKindIndexed =
    \case
      Trait{..} ->
        Trait traitName
          <$> toKindIndexed traitType

instance (ToKindIndexed t u) => ToKindIndexed (With t) (With u) where
  toKindIndexed =
    \case
      With traits t ->
        With
          <$> toKindIndexed traits
          <*> toKindIndexed t

instance (ToKindIndexed t u) => ToKindIndexed (Expression a t) (Expression a u) where
  toKindIndexed = traverse toKindIndexed

instance (ToKindIndexed t u) => ToKindIndexed (Pattern a t) (Pattern a u) where
  toKindIndexed = traverse toKindIndexed

instance (ToKindIndexed t u, ToKindIndexed (o k) (o Kind), Ord (o Kind)) => ToKindIndexed (Scheme o k t) (Scheme o Kind u) where
  toKindIndexed =
    \case
      Forall{..} ->
        Forall
          <$> toKindIndexed schemeTypeVariables
          <*> toKindIndexed schemeTraits
          <*> toKindIndexed schemeTypeBody

instance (ToKindIndexed t u) => ToKindIndexed (ProtoModule a k t) (ProtoModule a Kind u) where
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
