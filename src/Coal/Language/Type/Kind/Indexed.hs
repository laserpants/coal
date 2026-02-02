{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Language.Type.Kind.Indexed (ToKindIndexed (..)) where

import Coal.Common.Label (Label (..))
import Coal.Common.Supply (Supply (..), supplied)
import Coal.Language.Data.Constructor (DataConstructor (..))
import Coal.Language.Expression (Clause (..), CompiledClause (..), Expression (..))
import Coal.Language.Expression.Binding (Binding (..))
import Coal.Language.Expression.Choice (Choice (..), Guard (..))
import Coal.Language.Pattern (Pattern (..))
import Coal.Language.Trait (Trait (..), With (..))
import Coal.Language.Type (Parameter (..), Type (..), TypeIndex (..))
import Coal.Language.Type.Kind (Kind (..))
import Coal.Language.Type.Row (Row (..))
import Coal.Language.Type.Scheme (Scheme (..))
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule
import Control.Monad.Except (forM)
import Control.Monad.State (MonadState)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra (secondM)

class ToKindIndexed t u where
  toKindIndexed :: (MonadState s m, Supply s) => t -> m u

instance ToKindIndexed () () where
  toKindIndexed = pure

instance (ToKindIndexed t u) => ToKindIndexed (Maybe t) (Maybe u) where
  toKindIndexed = traverse toKindIndexed

instance (ToKindIndexed t u) => ToKindIndexed (Map k t) (Map k u) where
  toKindIndexed = traverse toKindIndexed

instance (ToKindIndexed t u) => ToKindIndexed [t] [u] where
  toKindIndexed = traverse toKindIndexed

instance (ToKindIndexed t u) => ToKindIndexed (NonEmpty t) (NonEmpty u) where
  toKindIndexed = traverse toKindIndexed

instance (ToKindIndexed t u, Ord u) => ToKindIndexed (Set t) (Set u) where
  toKindIndexed s = Set.fromList <$> toKindIndexed (Set.toList s)

instance ToKindIndexed (ProtoDefinition a k ()) (ProtoDefinition a Kind ()) where
  toKindIndexed =
    \case
      ProtoDType loc name def ->
        ProtoDType loc name <$> toKindIndexed def
      ProtoDTypeAlias loc name def ->
        ProtoDTypeAlias loc name <$> toKindIndexed def
      ProtoDFunction loc name def ->
        ProtoDFunction loc name <$> toKindIndexed def
      ProtoDFunctionGroup loc name defs ->
        ProtoDFunctionGroup loc name <$> traverse toKindIndexed defs
      ProtoDFold loc name def ->
        ProtoDFold loc name <$> toKindIndexed def
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

instance ToKindIndexed (ProtoFunctionDefinition a k ()) (ProtoFunctionDefinition a Kind ()) where
  toKindIndexed =
    \case
      ProtoFunctionDefinition{..} ->
        ProtoFunctionDefinition protoOfunctionDefinitionMetadata
          <$> toKindIndexed protoOfunctionDefinitionAnnotation
          <*> toKindIndexed protoOfunctionDefinitionType
          <*> toKindIndexed protoOfunctionDefinitionPatterns
          <*> toKindIndexed protoOfunctionDefinitionExpression

instance ToKindIndexed (ProtoLetDefinition a k ()) (ProtoLetDefinition a Kind ()) where
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
        ProtoTraitDefinition protoOtraitDefinitionMetadata protoOtraitDefinitionTraitName
          <$> toKindIndexed protoOtraitDefinitionConstraints
          <*> toKindIndexed protoOtraitDefinitionParameter
          <*> traverse (secondM toKindIndexed) protoOtraitDefinitionInterface

instance ToKindIndexed (ProtoFoldDefinition a k ()) (ProtoFoldDefinition a Kind ()) where
  toKindIndexed =
    \case
      ProtoFoldDefinition{..} ->
        ProtoFoldDefinition protoOfoldDefinitionMetadata
          <$> toKindIndexed protoOfoldDefinitionAnnotation
          <*> toKindIndexed protoOfoldDefinitionClauses

instance ToKindIndexed (ProtoInstanceDefinition a k ()) (ProtoInstanceDefinition a Kind ()) where
  toKindIndexed =
    \case
      ProtoInstanceDefinition{..} ->
        ProtoInstanceDefinition protoOinstanceDefinitionMetadata protoOinstanceDefinitionTraitName
          <$> toKindIndexed protoOinstanceDefinitionConstraints
          <*> toKindIndexed protoOinstanceDefinitionType
          <*> toKindIndexed protoOinstanceDefinitionImplementations

instance ToKindIndexed (ProtoAliasDefinition a k) (ProtoAliasDefinition a Kind) where
  toKindIndexed =
    \case
      ProtoAliasDefinition{..} ->
        ProtoAliasDefinition
          <$> toKindIndexed protoOaliasDefinitionParameters
          <*> toKindIndexed protoOaliasDefinitionType

instance (ToKindIndexed t u, ToKindIndexed (o k) (o Kind), Ord (o Kind)) => ToKindIndexed (DataConstructor o k t) (DataConstructor o Kind u) where
  toKindIndexed =
    \case
      DataConstructor{..} ->
        DataConstructor constructorName constructorArity
          <$> toKindIndexed constructorScheme

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

instance (ToKindIndexed t u) => ToKindIndexed (Label t) (Label u) where
  toKindIndexed =
    \case
      Label{..} ->
        Label
          <$> toKindIndexed labelTag
          <*> pure labelName

instance ToKindIndexed (Expression a k ()) (Expression a Kind ()) where
  toKindIndexed =
    \case
      EAnnotation a t e ->
        EAnnotation a <$> toKindIndexed t <*> toKindIndexed e
      EApplication a t e es ->
        EApplication a <$> toKindIndexed t <*> toKindIndexed e <*> toKindIndexed es
      ELambda a ps e ->
        ELambda a <$> toKindIndexed ps <*> toKindIndexed e
      ELet a bs e ->
        ELet a <$> toKindIndexed bs <*> toKindIndexed e
      ERecursiveLet a p e1 e2 ->
        ERecursiveLet a <$> toKindIndexed p <*> toKindIndexed e1 <*> toKindIndexed e2
      EVariable a ll ->
        EVariable a <$> toKindIndexed ll
      EConstructor a ll ->
        EConstructor a <$> toKindIndexed ll
      ELiteral a primitive ->
        pure (ELiteral a primitive)
      EIf a t e1 e2 e3 ->
        EIf a <$> toKindIndexed t <*> toKindIndexed e1 <*> toKindIndexed e2 <*> toKindIndexed e3
      EOperator a t op ->
        EOperator a <$> toKindIndexed t <*> pure op
      ERecord a t d e ->
        ERecord a <$> toKindIndexed t <*> toKindIndexed d <*> toKindIndexed e
      EListCons a t e1 e2 ->
        EListCons a <$> toKindIndexed t <*> toKindIndexed e1 <*> toKindIndexed e2
      EListLiteral a t es ->
        EListLiteral a <$> toKindIndexed t <*> toKindIndexed es
      ETuple a t es ->
        ETuple a <$> toKindIndexed t <*> toKindIndexed es
      EMatch a t e cs ->
        EMatch a <$> toKindIndexed t <*> toKindIndexed e <*> toKindIndexed cs
      ELambdaMatch a t cs ->
        ELambdaMatch a <$> toKindIndexed t <*> toKindIndexed cs
      ECompiledMatch a t e cs ->
        ECompiledMatch a <$> toKindIndexed t <*> toKindIndexed e <*> toKindIndexed cs
      EFold a t es cs ->
        EFold a <$> toKindIndexed t <*> toKindIndexed es <*> toKindIndexed cs
      ESelect a ll e ->
        ESelect a <$> toKindIndexed ll <*> toKindIndexed e
      EFocus a name ll1 ll2 e1 e2 ->
        EFocus a name <$> toKindIndexed ll1 <*> toKindIndexed ll2 <*> toKindIndexed e1 <*> toKindIndexed e2
      ETraitInstance a t trait ->
        ETraitInstance a <$> toKindIndexed t <*> toKindIndexed trait
      EFFICall a t ll es e ->
        EFFICall a <$> toKindIndexed t <*> toKindIndexed ll <*> toKindIndexed es <*> toKindIndexed e
      EDoBlock a is ->
        EDoBlock a <$> forM is (\(x, y) -> (,) <$> toKindIndexed x <*> toKindIndexed y)

instance ToKindIndexed (Clause a k ()) (Clause a Kind ()) where
  toKindIndexed =
    \case
      EClause a ps cs ->
        EClause a <$> toKindIndexed ps <*> toKindIndexed cs

instance ToKindIndexed (Choice Expression a k ()) (Choice Expression a Kind ()) where
  toKindIndexed =
    \case
      CPlain a gs e ->
        CPlain a <$> toKindIndexed gs <*> toKindIndexed e

instance ToKindIndexed (Guard Expression a k ()) (Guard Expression a Kind ()) where
  toKindIndexed =
    \case
      CGuard e ->
        CGuard <$> toKindIndexed e

instance ToKindIndexed (CompiledClause a k ()) (CompiledClause a Kind ()) where
  toKindIndexed =
    \case
      ECompiledClause a lls e ->
        ECompiledClause a <$> toKindIndexed lls <*> toKindIndexed e

instance ToKindIndexed (Binding Expression a k ()) (Binding Expression a Kind ()) where
  toKindIndexed =
    \case
      BPattern a p e ->
        BPattern a <$> toKindIndexed p <*> toKindIndexed e
      BFunction a name ps e ->
        BFunction a name <$> toKindIndexed ps <*> toKindIndexed e

instance ToKindIndexed (Pattern a k ()) (Pattern a Kind ()) where
  toKindIndexed =
    \case
      PAnnotation a t p ->
        PAnnotation a <$> toKindIndexed t <*> toKindIndexed p
      PAny a p ->
        PAny a <$> toKindIndexed p
      PVariable a ll ->
        PVariable a <$> toKindIndexed ll
      PConstructor a ll ps ->
        PConstructor a <$> toKindIndexed ll <*> toKindIndexed ps
      PInteger a t n ->
        PInteger a <$> toKindIndexed t <*> pure n
      PLiteral a primitive ->
        pure (PLiteral a primitive)
      PRecord a t d p ->
        PRecord a <$> toKindIndexed t <*> toKindIndexed d <*> toKindIndexed p
      PListCons a t p1 p2 ->
        PListCons a <$> toKindIndexed t <*> toKindIndexed p1 <*> toKindIndexed p2
      PListLiteral a t ps ->
        PListLiteral a <$> toKindIndexed t <*> toKindIndexed ps
      PTuple a t ps ->
        PTuple a <$> toKindIndexed t <*> toKindIndexed ps
      POr a t p1 p2 ->
        POr a <$> toKindIndexed t <*> toKindIndexed p1 <*> toKindIndexed p2
      PAs a ll p ->
        PAs a <$> toKindIndexed ll <*> toKindIndexed p
      PShorthand a ll ->
        PShorthand a <$> toKindIndexed ll
      PAtVariable a ll ->
        PAtVariable a <$> toKindIndexed ll
      PNamedFold a name ll ->
        PNamedFold a name <$> toKindIndexed ll
      PTraitInstance a t trait ->
        PTraitInstance a <$> toKindIndexed t <*> toKindIndexed trait

instance (ToKindIndexed t u, ToKindIndexed (o k) (o Kind), Ord (o Kind)) => ToKindIndexed (Scheme o k t) (Scheme o Kind u) where
  toKindIndexed =
    \case
      Forall{..} ->
        Forall
          <$> toKindIndexed schemeTypeVariables
          <*> toKindIndexed schemeTraits
          <*> toKindIndexed schemeTypeBody

instance ToKindIndexed (ProtoModule a k ()) (ProtoModule a Kind ()) where
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

instance ToKindIndexed (Type Parameter ()) (Type Parameter ()) where
  toKindIndexed = pure

instance ToKindIndexed (Type Parameter k) (Type Parameter Kind) where
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

instance ToKindIndexed (Row Parameter k (Type Parameter k)) (Row Parameter Kind (Type Parameter Kind)) where
  toKindIndexed =
    \case
      RExtend name t r ->
        RExtend name <$> toKindIndexed t <*> toKindIndexed r
      RVariable v ->
        RVariable <$> toKindIndexed v
      RNil ->
        pure RNil

kVar :: (MonadState s m, Supply s) => m Kind
kVar = supplied KVar
