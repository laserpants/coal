{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.TypeSystem.Kind.Substitution (
  KindSubstitutable (..),
  KindSubstitution (..),
) where

import Coal.Common.Environment (Environment (..), mapEnvironment)
import Coal.Compiler.Build (Build (..), InstanceMap)
import Coal.Compiler.Build.NameEntry
import Coal.Language.Data.Constructor (DataConstructor (..))
import Coal.Language.Definition
import Coal.Language.Expression (Clause (..), CompiledClause (..), Expression (..))
import Coal.Language.Expression.Binding (Binding (..))
import Coal.Language.Expression.Choice (Choice (..), Guard (..))
import Coal.Language.Module (Module (..))
import Coal.Language.Pattern (Pattern (..))
import Coal.Language.Trait (Qualified (..), Trait (..))
import Coal.Language.Type (Parameter (..), Type (..), TypeIndex (..))
import Coal.Language.Type.Kind (Kind (..))
import Coal.Language.Type.Row (Row (..))
import Coal.Language.Type.Scheme (Scheme (..))
import Coal.TypeSystem.Kind.Constraint (KindConstraint (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Extras (Name)

newtype KindSubstitution = KindSubstitution
  {kindSubstitutionMap :: Map Int Kind}
  deriving (Show, Eq, Ord)

instance Semigroup KindSubstitution where
  s1 <> s2 = KindSubstitution (s3 <> kindSubstitutionMap s1)
   where
    s3 = applyKinds s1 (kindSubstitutionMap s2)

instance Monoid KindSubstitution where
  mempty = KindSubstitution mempty

class KindSubstitutable k where
  applyKinds :: KindSubstitution -> k -> k
  replaceVariables :: k -> k

instance (KindSubstitutable k) => KindSubstitutable [k] where
  applyKinds = fmap . applyKinds
  replaceVariables = fmap replaceVariables

instance (KindSubstitutable k) => KindSubstitutable (NonEmpty k) where
  applyKinds = fmap . applyKinds
  replaceVariables = fmap replaceVariables

instance (KindSubstitutable k) => KindSubstitutable (Maybe k) where
  applyKinds = fmap . applyKinds
  replaceVariables = fmap replaceVariables

instance (KindSubstitutable k) => KindSubstitutable (Map Name k) where
  applyKinds = fmap . applyKinds
  replaceVariables = fmap replaceVariables

instance (KindSubstitutable k) => KindSubstitutable (Environment k) where
  applyKinds = mapEnvironment . applyKinds
  replaceVariables = mapEnvironment replaceVariables

instance (Ord k, KindSubstitutable k) => KindSubstitutable (Set k) where
  applyKinds = Set.map . applyKinds
  replaceVariables = Set.map replaceVariables

instance (KindSubstitutable n, KindSubstitutable k) => KindSubstitutable (n, k) where
  applyKinds sub (a, b) = (applyKinds sub a, applyKinds sub b)
  replaceVariables = fmap replaceVariables

instance (Ord (o k), KindSubstitutable (o k), KindSubstitutable t) => KindSubstitutable (Scheme o k t) where
  applyKinds sub =
    \case
      Forall{..} ->
        Forall
          { schemeTypeVariables = applyKinds sub schemeTypeVariables
          , schemeTraits = applyKinds sub schemeTraits
          , schemeTypeBody = applyKinds sub schemeTypeBody
          }
  replaceVariables =
    \case
      Forall{..} ->
        Forall
          { schemeTypeVariables = replaceVariables schemeTypeVariables
          , schemeTraits = replaceVariables schemeTraits
          , schemeTypeBody = replaceVariables schemeTypeBody
          }

instance (KindSubstitutable k) => KindSubstitutable (Trait k) where
  applyKinds = fmap . applyKinds
  replaceVariables = fmap replaceVariables

instance KindSubstitutable (Map Int Kind) where
  applyKinds = fmap . applyKinds
  replaceVariables = fmap replaceVariables

instance KindSubstitutable KindConstraint where
  applyKinds sub =
    \case
      KEquality k1 k2 ->
        KEquality (applyKinds sub k1) (applyKinds sub k2)
  replaceVariables =
    \case
      KEquality k1 k2 ->
        KEquality (replaceVariables k1) (replaceVariables k2)

instance KindSubstitutable Kind where
  applyKinds sub =
    \case
      KArrow k1 k2 ->
        KArrow (applyKinds sub k1) (applyKinds sub k2)
      KVariable n ->
        fromMaybe (KVariable n) (Map.lookup n (kindSubstitutionMap sub))
      k ->
        k
  replaceVariables =
    \case
      KArrow k1 k2 ->
        KArrow (replaceVariables k1) (replaceVariables k2)
      KVariable{} ->
        KType
      k ->
        k

instance (KindSubstitutable k) => KindSubstitutable (Parameter k) where
  applyKinds sub =
    \case
      Parameter k name ->
        Parameter (applyKinds sub k) name
  replaceVariables =
    \case
      Parameter k name ->
        Parameter (replaceVariables k) name

instance (KindSubstitutable k) => KindSubstitutable (TypeIndex k) where
  applyKinds sub =
    \case
      TypeIndex{..} ->
        TypeIndex
          { typeIndexKind = applyKinds sub typeIndexKind
          , ..
          }
  replaceVariables =
    \case
      TypeIndex{..} ->
        TypeIndex
          { typeIndexKind = replaceVariables typeIndexKind
          , ..
          }

instance (KindSubstitutable (o Kind)) => KindSubstitutable (Type o Kind) where
  applyKinds sub =
    \case
      TApplication k t1 t2 ->
        TApplication (applyKinds sub k) (applyKinds sub t1) (applyKinds sub t2)
      TArrow t1 t2 ->
        TArrow (applyKinds sub t1) (applyKinds sub t2)
      TConstructor k name ->
        TConstructor (applyKinds sub k) name
      TIntrinsic i ->
        TIntrinsic i
      TRecord t ->
        TRecord (applyKinds sub t)
      TRow row ->
        TRow (applyKinds sub row)
      TVariable param ->
        TVariable (applyKinds sub param)
      TAlias name ts t ->
        TAlias name (fmap (applyKinds sub) ts) (applyKinds sub t)
  replaceVariables =
    \case
      TApplication k t1 t2 ->
        TApplication (replaceVariables k) (replaceVariables t1) (replaceVariables t2)
      TArrow t1 t2 ->
        TArrow (replaceVariables t1) (replaceVariables t2)
      TConstructor k name ->
        TConstructor (replaceVariables k) name
      TIntrinsic i ->
        TIntrinsic i
      TRecord t ->
        TRecord (replaceVariables t)
      TRow row ->
        TRow (replaceVariables row)
      TVariable var ->
        TVariable (replaceVariables var)
      TAlias name ts t ->
        TAlias name (fmap replaceVariables ts) (replaceVariables t)

instance (KindSubstitutable n, KindSubstitutable (o n), KindSubstitutable k) => KindSubstitutable (Row o n k) where
  applyKinds sub =
    \case
      RExtend name t row ->
        RExtend name (applyKinds sub t) (applyKinds sub row)
      RVariable var ->
        RVariable (applyKinds sub var)
      RNil ->
        RNil
  replaceVariables =
    \case
      RExtend name t row ->
        RExtend name (replaceVariables t) (replaceVariables row)
      RVariable var ->
        RVariable (replaceVariables var)
      RNil ->
        RNil

instance (KindSubstitutable k, KindSubstitutable t, Ord k) => KindSubstitutable (DataConstructor Parameter k t) where
  applyKinds sub DataConstructor{..} =
    DataConstructor{constructorScheme = applyKinds sub constructorScheme, ..}
  replaceVariables DataConstructor{..} =
    DataConstructor{constructorScheme = replaceVariables constructorScheme, ..}

instance KindSubstitutable (DataConstructor TypeIndex Kind (Type TypeIndex Kind)) where
  applyKinds sub DataConstructor{..} =
    DataConstructor{constructorScheme = applyKinds sub constructorScheme, ..}
  replaceVariables DataConstructor{..} =
    DataConstructor{constructorScheme = replaceVariables constructorScheme, ..}

instance KindSubstitutable (Module a Kind ()) where
  applyKinds sub Module{..} =
    Module
      { moduleDefinitions = applyKinds sub moduleDefinitions
      , ..
      }
  replaceVariables Module{..} =
    Module
      { moduleDefinitions = replaceVariables moduleDefinitions
      , ..
      }

instance KindSubstitutable (Definition a Kind ()) where
  applyKinds sub =
    \case
      DType a name def ->
        DType a name (applyKinds sub def)
      DTypeAlias a name def ->
        DTypeAlias a name (applyKinds sub def)
      DFunction a name def ->
        DFunction a name (applyKinds sub def)
      DFunctionGroup a name FunctionGroupDefinition{..} ->
        DFunctionGroup
          a
          name
          FunctionGroupDefinition
            { functionGroupDefinitionBranches = applyKinds sub <$> functionGroupDefinitionBranches
            , ..
            }
      DFold a name def ->
        DFold a name (applyKinds sub def)
      DLet a name def ->
        DLet a name (applyKinds sub def)
      def@DImport{} ->
        def
      def@DNamespaceImport{} ->
        def
      DTrait a name def ->
        DTrait a name (applyKinds sub def)
      DInstance a def ->
        DInstance a (applyKinds sub def)
  replaceVariables =
    \case
      DType a name def ->
        DType a name (replaceVariables def)
      DTypeAlias a name def ->
        DTypeAlias a name (replaceVariables def)
      DFunction a name def ->
        DFunction a name (replaceVariables def)
      DFunctionGroup a name FunctionGroupDefinition{..} ->
        DFunctionGroup
          a
          name
          FunctionGroupDefinition
            { functionGroupDefinitionBranches = replaceVariables <$> functionGroupDefinitionBranches
            , ..
            }
      DFold a name def ->
        DFold a name (replaceVariables def)
      DLet a name def ->
        DLet a name (replaceVariables def)
      def@DImport{} ->
        def
      def@DNamespaceImport{} ->
        def
      DTrait a name def ->
        DTrait a name (replaceVariables def)
      DInstance a def ->
        DInstance a (replaceVariables def)

instance KindSubstitutable (Expression a Kind ()) where
  applyKinds sub =
    \case
      EAnnotation a t e ->
        EAnnotation a t (applyKinds sub e)
      EApplication a () e es ->
        EApplication a () (applyKinds sub e) (applyKinds sub es)
      ELambda a ps e ->
        ELambda a (applyKinds sub ps) (applyKinds sub e)
      ELet a bs e ->
        ELet a (applyKinds sub bs) (applyKinds sub e)
      ERecursiveLet a p e1 e2 ->
        ERecursiveLet a (applyKinds sub p) (applyKinds sub e1) (applyKinds sub e2)
      EIf a () e1 e2 e3 ->
        EIf a () (applyKinds sub e1) (applyKinds sub e2) (applyKinds sub e3)
      ERecord a () d e ->
        ERecord a () (applyKinds sub d) (applyKinds sub e)
      EListCons a () e1 e2 ->
        EListCons a () (applyKinds sub e1) (applyKinds sub e2)
      EListLiteral a () es ->
        EListLiteral a () (applyKinds sub es)
      ETuple a () es ->
        ETuple a () (applyKinds sub es)
      EMatch a () e cs ->
        EMatch a () (applyKinds sub e) (applyKinds sub cs)
      ELambdaMatch a () cs ->
        ELambdaMatch a () (applyKinds sub cs)
      ECompiledMatch a () e cs ->
        ECompiledMatch a () (applyKinds sub e) (applyKinds sub cs)
      EFold a () es cs ->
        EFold a () (applyKinds sub es) (applyKinds sub cs)
      ESelect a ll e ->
        ESelect a ll (applyKinds sub e)
      EFocus a name ll1 ll2 e1 e2 ->
        EFocus a name ll1 ll2 (applyKinds sub e1) (applyKinds sub e2)
      EFFICall a () ll es e ->
        EFFICall a () ll (applyKinds sub es) (applyKinds sub e)
      EDoBlock a is ->
        EDoBlock a (applyKinds sub is)
      e ->
        e
  replaceVariables =
    \case
      EAnnotation a t e ->
        EAnnotation a t (replaceVariables e)
      EApplication a () e es ->
        EApplication a () (replaceVariables e) (replaceVariables es)
      ELambda a ps e ->
        ELambda a (replaceVariables ps) (replaceVariables e)
      ELet a bs e ->
        ELet a (replaceVariables bs) (replaceVariables e)
      ERecursiveLet a p e1 e2 ->
        ERecursiveLet a (replaceVariables p) (replaceVariables e1) (replaceVariables e2)
      EIf a () e1 e2 e3 ->
        EIf a () (replaceVariables e1) (replaceVariables e2) (replaceVariables e3)
      ERecord a () d e ->
        ERecord a () (replaceVariables d) (replaceVariables e)
      EListCons a () e1 e2 ->
        EListCons a () (replaceVariables e1) (replaceVariables e2)
      EListLiteral a () es ->
        EListLiteral a () (replaceVariables es)
      ETuple a () es ->
        ETuple a () (replaceVariables es)
      EMatch a () e cs ->
        EMatch a () (replaceVariables e) (replaceVariables cs)
      ELambdaMatch a () cs ->
        ELambdaMatch a () (replaceVariables cs)
      ECompiledMatch a () e cs ->
        ECompiledMatch a () (replaceVariables e) (replaceVariables cs)
      EFold a () es cs ->
        EFold a () (replaceVariables es) (replaceVariables cs)
      ESelect a ll e ->
        ESelect a ll (replaceVariables e)
      EFocus a name ll1 ll2 e1 e2 ->
        EFocus a name ll1 ll2 (replaceVariables e1) (replaceVariables e2)
      EFFICall a () ll es e ->
        EFFICall a () ll (replaceVariables es) (replaceVariables e)
      EDoBlock a is ->
        EDoBlock a (replaceVariables is)
      e ->
        e

instance KindSubstitutable (Binding Expression a Kind ()) where
  applyKinds sub =
    \case
      BPattern a p e ->
        BPattern a (applyKinds sub p) (applyKinds sub e)
      BFunction a name ps e ->
        BFunction a name (applyKinds sub ps) (applyKinds sub e)
  replaceVariables =
    \case
      BPattern a p e ->
        BPattern a (replaceVariables p) (replaceVariables e)
      BFunction a name ps e ->
        BFunction a name (replaceVariables ps) (replaceVariables e)

instance KindSubstitutable (Pattern a Kind ()) where
  applyKinds sub =
    \case
      PAnnotation a t p ->
        PAnnotation a (applyKinds sub t) (applyKinds sub p)
      PConstructor a t ps ->
        PConstructor a t (applyKinds sub ps)
      PRecord a () d p ->
        PRecord a () (applyKinds sub d) (applyKinds sub p)
      PListCons a () p1 p2 ->
        PListCons a () (applyKinds sub p1) (applyKinds sub p2)
      PListLiteral a () ps ->
        PListLiteral a () (applyKinds sub ps)
      PTuple a () ps ->
        PTuple a () (applyKinds sub ps)
      POr a () p1 p2 ->
        POr a () (applyKinds sub p1) (applyKinds sub p2)
      PAs a t p ->
        PAs a t (applyKinds sub p)
      p ->
        p
  replaceVariables =
    \case
      PAnnotation a t p ->
        PAnnotation a (replaceVariables t) (replaceVariables p)
      PConstructor a t ps ->
        PConstructor a t (replaceVariables ps)
      PRecord a () d p ->
        PRecord a () (replaceVariables d) (replaceVariables p)
      PListCons a () p1 p2 ->
        PListCons a () (replaceVariables p1) (replaceVariables p2)
      PListLiteral a () ps ->
        PListLiteral a () (replaceVariables ps)
      PTuple a () ps ->
        PTuple a () (replaceVariables ps)
      POr a () p1 p2 ->
        POr a () (replaceVariables p1) (replaceVariables p2)
      PAs a t p ->
        PAs a t (replaceVariables p)
      p ->
        p

instance KindSubstitutable (TypeDefinition a Kind ()) where
  applyKinds sub TypeDefinition{..} =
    TypeDefinition
      { typeDefinitionParameters = applyKinds sub typeDefinitionParameters
      , typeDefinitionConstructors = applyKinds sub typeDefinitionConstructors
      }
  replaceVariables TypeDefinition{..} =
    TypeDefinition
      { typeDefinitionParameters = replaceVariables typeDefinitionParameters
      , typeDefinitionConstructors = replaceVariables typeDefinitionConstructors
      }

instance KindSubstitutable (FunctionDefinition a Kind ()) where
  applyKinds sub FunctionDefinition{..} =
    FunctionDefinition
      { functionDefinitionAnnotation = applyKinds sub functionDefinitionAnnotation
      , functionDefinitionConstraints = applyKinds sub functionDefinitionConstraints
      , functionDefinitionPatterns = applyKinds sub functionDefinitionPatterns
      , functionDefinitionExpression = applyKinds sub functionDefinitionExpression
      , ..
      }
  replaceVariables FunctionDefinition{..} =
    FunctionDefinition
      { functionDefinitionAnnotation = replaceVariables functionDefinitionAnnotation
      , functionDefinitionConstraints = replaceVariables functionDefinitionConstraints
      , functionDefinitionPatterns = replaceVariables functionDefinitionPatterns
      , functionDefinitionExpression = replaceVariables functionDefinitionExpression
      , ..
      }

instance KindSubstitutable (LetDefinition a Kind ()) where
  applyKinds sub LetDefinition{..} =
    LetDefinition
      { letDefinitionAnnotation = applyKinds sub letDefinitionAnnotation
      , letDefinitionConstraints = applyKinds sub letDefinitionConstraints
      , letDefinitionExpression = applyKinds sub letDefinitionExpression
      , ..
      }
  replaceVariables LetDefinition{..} =
    LetDefinition
      { letDefinitionAnnotation = replaceVariables letDefinitionAnnotation
      , letDefinitionConstraints = replaceVariables letDefinitionConstraints
      , letDefinitionExpression = replaceVariables letDefinitionExpression
      , ..
      }

instance KindSubstitutable (TraitDefinition a Kind) where
  applyKinds sub TraitDefinition{..} =
    TraitDefinition
      { traitDefinitionConstraints = applyKinds sub traitDefinitionConstraints
      , traitDefinitionParameter = applyKinds sub traitDefinitionParameter
      , traitDefinitionInterface = applyKinds sub traitDefinitionInterface
      , ..
      }
  replaceVariables TraitDefinition{..} =
    TraitDefinition
      { traitDefinitionConstraints = replaceVariables traitDefinitionConstraints
      , traitDefinitionParameter = replaceVariables traitDefinitionParameter
      , traitDefinitionInterface = replaceVariables traitDefinitionInterface
      , ..
      }

instance KindSubstitutable (TraitDefinitionInterfaceEntry Kind) where
  applyKinds sub TraitDefinitionInterfaceEntry{..} =
    TraitDefinitionInterfaceEntry
      traitDefinitionInterfaceEntryName
      (applyKinds sub traitDefinitionInterfaceEntryScheme)
  replaceVariables TraitDefinitionInterfaceEntry{..} =
    TraitDefinitionInterfaceEntry
      traitDefinitionInterfaceEntryName
      (replaceVariables traitDefinitionInterfaceEntryScheme)

instance KindSubstitutable (InstanceDefinition a Kind ()) where
  applyKinds sub InstanceDefinition{..} =
    InstanceDefinition
      { instanceDefinitionConstraints = applyKinds sub instanceDefinitionConstraints
      , instanceDefinitionType = applyKinds sub instanceDefinitionType
      , instanceDefinitionImplementations = applyKinds sub instanceDefinitionImplementations
      , ..
      }
  replaceVariables InstanceDefinition{..} =
    InstanceDefinition
      { instanceDefinitionConstraints = replaceVariables instanceDefinitionConstraints
      , instanceDefinitionType = replaceVariables instanceDefinitionType
      , instanceDefinitionImplementations = replaceVariables instanceDefinitionImplementations
      , ..
      }

instance KindSubstitutable (FoldDefinition a Kind ()) where
  applyKinds sub FoldDefinition{..} =
    FoldDefinition
      { foldDefinitionAnnotation = applyKinds sub foldDefinitionAnnotation
      , foldDefinitionConstraints = applyKinds sub foldDefinitionConstraints
      , foldDefinitionClauses = applyKinds sub foldDefinitionClauses
      , ..
      }
  replaceVariables FoldDefinition{..} =
    FoldDefinition
      { foldDefinitionAnnotation = replaceVariables foldDefinitionAnnotation
      , foldDefinitionConstraints = replaceVariables foldDefinitionConstraints
      , foldDefinitionClauses = replaceVariables foldDefinitionClauses
      , ..
      }

instance KindSubstitutable (AliasDefinition a Kind) where
  applyKinds sub AliasDefinition{..} =
    AliasDefinition
      { aliasDefinitionParameters = applyKinds sub aliasDefinitionParameters
      , aliasDefinitionType = applyKinds sub aliasDefinitionType
      }
  replaceVariables AliasDefinition{..} =
    AliasDefinition
      { aliasDefinitionParameters = replaceVariables aliasDefinitionParameters
      , aliasDefinitionType = replaceVariables aliasDefinitionType
      }

instance KindSubstitutable (Clause a Kind ()) where
  applyKinds sub =
    \case
      EClause a p cs ->
        EClause a (applyKinds sub p) (applyKinds sub cs)
  replaceVariables =
    \case
      EClause a p cs ->
        EClause a (replaceVariables p) (replaceVariables cs)

instance KindSubstitutable (CompiledClause a Kind ()) where
  applyKinds sub =
    \case
      ECompiledClause a lls e ->
        ECompiledClause a lls (applyKinds sub e)
  replaceVariables =
    \case
      ECompiledClause a lls e ->
        ECompiledClause a lls (replaceVariables e)

instance KindSubstitutable (Choice Expression a Kind ()) where
  applyKinds sub =
    \case
      CPlain a gs e ->
        CPlain a (applyKinds sub gs) (applyKinds sub e)
  replaceVariables =
    \case
      CPlain a gs e ->
        CPlain a (replaceVariables gs) (replaceVariables e)

instance KindSubstitutable (Guard Expression a Kind ()) where
  applyKinds sub =
    \case
      CGuard e ->
        CGuard (applyKinds sub e)
  replaceVariables =
    \case
      CGuard e ->
        CGuard (replaceVariables e)

instance KindSubstitutable (Qualified (Type Parameter Kind)) where
  applyKinds sub =
    \case
      With ts t ->
        With (applyKinds sub ts) (applyKinds sub t)
  replaceVariables =
    \case
      With ts t ->
        With (replaceVariables ts) (replaceVariables t)

instance KindSubstitutable (Build a) where
  applyKinds sub =
    \case
      Build{..} ->
        Build
          { buildNames = applyKinds sub buildNames
          , buildDataConstructors = applyKinds sub buildDataConstructors
          , buildTypeConstructors = applyKinds sub buildTypeConstructors
          , buildTraits = applyKinds sub buildTraits
          , buildInstances = mapEnvironment (Map.mapKeys (applyKinds sub) . Map.map (applyKinds sub)) buildInstances
          , buildAliases = applyKinds sub buildAliases
          , ..
          }
  replaceVariables =
    \case
      Build{..} ->
        Build
          { buildNames = replaceVariables buildNames
          , buildDataConstructors = replaceVariables buildDataConstructors
          , buildTypeConstructors = replaceVariables buildTypeConstructors
          , buildTraits = replaceVariables buildTraits
          , buildInstances = mapEnvironment (Map.mapKeys replaceVariables . Map.map replaceVariables) buildInstances
          , buildAliases = replaceVariables buildAliases
          , ..
          }

instance (KindSubstitutable a) => KindSubstitutable (InstanceMap a) where
  applyKinds = Map.map . applyKinds
  replaceVariables = Map.map replaceVariables

instance KindSubstitutable (DataConstructorEntry a) where
  applyKinds sub =
    \case
      DataConstructorEntry{..} ->
        DataConstructorEntry
          { dataConstructorEntryConstructor =
              applyKinds sub dataConstructorEntryConstructor
          , ..
          }
  replaceVariables =
    \case
      DataConstructorEntry{..} ->
        DataConstructorEntry
          { dataConstructorEntryConstructor =
              replaceVariables dataConstructorEntryConstructor
          , ..
          }

instance KindSubstitutable (TypeConstructorEntry a) where
  applyKinds sub =
    \case
      TypeConstructorEntry{..} ->
        TypeConstructorEntry
          { typeConstructorEntryKind =
              applyKinds sub typeConstructorEntryKind
          , ..
          }
  replaceVariables =
    \case
      TypeConstructorEntry{..} ->
        TypeConstructorEntry
          { typeConstructorEntryKind =
              replaceVariables typeConstructorEntryKind
          , ..
          }

instance KindSubstitutable (TraitEntry a) where
  applyKinds sub =
    \case
      TraitEntry{..} ->
        TraitEntry
          { traitEntryParameter = applyKinds sub traitEntryParameter
          , traitEntryConstraints = applyKinds sub traitEntryConstraints
          , traitEntryInterface = applyKinds sub traitEntryInterface
          , ..
          }
  replaceVariables =
    \case
      TraitEntry{..} ->
        TraitEntry
          { traitEntryParameter = replaceVariables traitEntryParameter
          , traitEntryConstraints = replaceVariables traitEntryConstraints
          , traitEntryInterface = replaceVariables traitEntryInterface
          , ..
          }

instance KindSubstitutable (InstanceEntry a) where
  applyKinds sub =
    \case
      InstanceEntry{..} ->
        InstanceEntry
          { instanceEntryType = applyKinds sub instanceEntryType
          , instanceEntryIndexedType = applyKinds sub instanceEntryIndexedType
          , instanceEntryTypeSchemes = applyKinds sub instanceEntryTypeSchemes
          , ..
          }
  replaceVariables =
    \case
      InstanceEntry{..} ->
        InstanceEntry
          { instanceEntryType = replaceVariables instanceEntryType
          , instanceEntryIndexedType = replaceVariables instanceEntryIndexedType
          , instanceEntryTypeSchemes = replaceVariables instanceEntryTypeSchemes
          , ..
          }

instance KindSubstitutable (AliasEntry a) where
  applyKinds sub =
    \case
      AliasEntry{..} ->
        AliasEntry
          { aliasEntryType = applyKinds sub aliasEntryType
          , aliasEntryParams = applyKinds sub aliasEntryParams
          , ..
          }
  replaceVariables =
    \case
      AliasEntry{..} ->
        AliasEntry
          { aliasEntryType = replaceVariables aliasEntryType
          , aliasEntryParams = replaceVariables aliasEntryParams
          , ..
          }

instance KindSubstitutable NameEntry where
  applyKinds sub =
    \case
      NName n s ->
        NName n (applyKinds sub s)
      NType n k ->
        NType n (applyKinds sub k)
      entry ->
        entry
  replaceVariables =
    \case
      NName n s ->
        NName n (replaceVariables s)
      NType n k ->
        NType n (replaceVariables k)
      entry ->
        entry
