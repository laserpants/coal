{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Coal.Language.Type.Kind.Indexed

Conversion of language constructs to kind-indexed representations.
-}
module Coal.Language.Type.Kind.Indexed (ToKindIndexed (..)) where

import Coal.Common.Label (Label (..))
import Coal.Common.Supply (Supply (..), supplied)
import Coal.Language.Data.Constructor (DataConstructor (..))
import Coal.Language.Definition
import Coal.Language.Expression (Clause (..), CompiledClause (..), Expression (..))
import Coal.Language.Expression.Binding (Binding (..))
import Coal.Language.Expression.Choice (Choice (..), Guard (..))
import Coal.Language.Module
import Coal.Language.Pattern (Pattern (..))
import Coal.Language.Trait (Qualified (..), Trait (..))
import Coal.Language.Type (Parameter (..), Type (..), TypeIndex (..))
import Coal.Language.Type.Kind (Kind (..))
import Coal.Language.Type.Row (Row (..))
import Coal.Language.Type.Scheme (Scheme (..))
import Control.Monad.Except (forM)
import Control.Monad.State (MonadState)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as Set

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

instance ToKindIndexed (Definition a k ()) (Definition a Kind ()) where
  toKindIndexed =
    \case
      DType loc name def ->
        DType loc name <$> toKindIndexed def
      DTypeAlias loc name def ->
        DTypeAlias loc name <$> toKindIndexed def
      DFunction loc name def ->
        DFunction loc name <$> toKindIndexed def
      DFunctionGroup loc name defs ->
        DFunctionGroup loc name <$> traverse toKindIndexed defs
      DFold loc name def ->
        DFold loc name <$> toKindIndexed def
      DLet loc name def ->
        DLet loc name <$> toKindIndexed def
      DImport loc path imports ->
        pure $ DImport loc path imports
      DNamespaceImport loc path ->
        pure $ DNamespaceImport loc path
      DTrait loc name def ->
        DTrait loc name <$> toKindIndexed def
      DInstance loc def ->
        DInstance loc <$> toKindIndexed def

instance ToKindIndexed (TypeDefinition a k t) (TypeDefinition a Kind u) where
  toKindIndexed =
    \case
      TypeDefinition{..} ->
        TypeDefinition
          <$> toKindIndexed typeDefinitionParameters
          <*> toKindIndexed typeDefinitionConstructors

instance ToKindIndexed (FunctionDefinition a k ()) (FunctionDefinition a Kind ()) where
  toKindIndexed =
    \case
      FunctionDefinition{..} ->
        FunctionDefinition functionDefinitionMetadata
          <$> toKindIndexed functionDefinitionAnnotation
          <*> toKindIndexed functionDefinitionType
          <*> toKindIndexed functionDefinitionPatterns
          <*> toKindIndexed functionDefinitionExpression

instance ToKindIndexed (LetDefinition a k ()) (LetDefinition a Kind ()) where
  toKindIndexed =
    \case
      LetDefinition{..} ->
        LetDefinition letDefinitionMetadata
          <$> toKindIndexed letDefinitionAnnotation
          <*> toKindIndexed letDefinitionType
          <*> toKindIndexed letDefinitionExpression

instance ToKindIndexed (TraitDefinition a k) (TraitDefinition a Kind) where
  toKindIndexed =
    \case
      TraitDefinition{..} ->
        TraitDefinition traitDefinitionMetadata traitDefinitionTraitName
          <$> toKindIndexed traitDefinitionConstraints
          <*> toKindIndexed traitDefinitionParameter
          <*> toKindIndexed traitDefinitionInterface

instance ToKindIndexed (TraitDefinitionInterfaceEntry k) (TraitDefinitionInterfaceEntry Kind) where
  toKindIndexed =
    \case
      TraitDefinitionInterfaceEntry{..} ->
        TraitDefinitionInterfaceEntry traitDefinitionInterfaceEntryName
          <$> toKindIndexed traitDefinitionInterfaceEntryScheme

instance ToKindIndexed (FoldDefinition a k ()) (FoldDefinition a Kind ()) where
  toKindIndexed =
    \case
      FoldDefinition{..} ->
        FoldDefinition foldDefinitionMetadata
          <$> toKindIndexed foldDefinitionAnnotation
          <*> toKindIndexed foldDefinitionClauses

instance ToKindIndexed (InstanceDefinition a k ()) (InstanceDefinition a Kind ()) where
  toKindIndexed =
    \case
      InstanceDefinition{..} ->
        InstanceDefinition instanceDefinitionMetadata instanceDefinitionTraitName
          <$> toKindIndexed instanceDefinitionConstraints
          <*> toKindIndexed instanceDefinitionType
          <*> toKindIndexed instanceDefinitionImplementations

instance ToKindIndexed (AliasDefinition a k) (AliasDefinition a Kind) where
  toKindIndexed =
    \case
      AliasDefinition{..} ->
        AliasDefinition
          <$> toKindIndexed aliasDefinitionParameters
          <*> toKindIndexed aliasDefinitionType

instance (ToKindIndexed t u, ToKindIndexed (o k) (o Kind), Ord (o Kind), Ord u) => ToKindIndexed (DataConstructor o k t) (DataConstructor o Kind u) where
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

instance (ToKindIndexed t u) => ToKindIndexed (Qualified t) (Qualified u) where
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

instance (ToKindIndexed t u, ToKindIndexed (o k) (o Kind), Ord (o Kind), Ord u) => ToKindIndexed (Scheme o k t) (Scheme o Kind u) where
  toKindIndexed =
    \case
      Forall{..} ->
        Forall
          <$> toKindIndexed schemeTypeVariables
          <*> toKindIndexed schemeTraits
          <*> toKindIndexed schemeTypeBody

instance ToKindIndexed (Module a k ()) (Module a Kind ()) where
  toKindIndexed =
    \case
      Module{..} -> do
        newModuleDefinitions <- traverse toKindIndexed moduleDefinitions
        pure $
          Module
            { moduleDefinitions = newModuleDefinitions
            , ..
            }

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
kVar = supplied KVariable
