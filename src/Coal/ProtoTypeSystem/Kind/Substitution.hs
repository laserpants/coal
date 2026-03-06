{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.ProtoTypeSystem.Kind.Substitution (
  ProtoKindSubstitutable (..),
  ProtoKindSubstitution (..),
) where

import Coal.Common.Environment (Environment (..), mapEnvironment)
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
import Coal.ProtoCompiler.ProtoBuild (InstanceMap, ProtoBuild (..))
import Coal.ProtoCompiler.ProtoBuild.ProtoNameEntry
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule
import Coal.ProtoTypeSystem.Kind.Constraint (ProtoKindConstraint (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Extras (Name)

newtype ProtoKindSubstitution = ProtoKindSubstitution
  {kindSubstitutionMap :: Map Int Kind}
  deriving (Show, Eq, Ord)

instance Semigroup ProtoKindSubstitution where
  s1 <> s2 = ProtoKindSubstitution (s3 <> kindSubstitutionMap s1)
   where
    s3 = protoOapplyKinds s1 (kindSubstitutionMap s2)

instance Monoid ProtoKindSubstitution where
  mempty = ProtoKindSubstitution mempty

class ProtoKindSubstitutable k where
  protoOapplyKinds :: ProtoKindSubstitution -> k -> k
  protoOreplaceVariables :: k -> k

instance (ProtoKindSubstitutable k) => ProtoKindSubstitutable [k] where
  protoOapplyKinds = fmap . protoOapplyKinds
  protoOreplaceVariables = fmap protoOreplaceVariables

instance (ProtoKindSubstitutable k) => ProtoKindSubstitutable (NonEmpty k) where
  protoOapplyKinds = fmap . protoOapplyKinds
  protoOreplaceVariables = fmap protoOreplaceVariables

instance (ProtoKindSubstitutable k) => ProtoKindSubstitutable (Maybe k) where
  protoOapplyKinds = fmap . protoOapplyKinds
  protoOreplaceVariables = fmap protoOreplaceVariables

instance (ProtoKindSubstitutable k) => ProtoKindSubstitutable (Map Name k) where
  protoOapplyKinds = fmap . protoOapplyKinds
  protoOreplaceVariables = fmap protoOreplaceVariables

instance (ProtoKindSubstitutable k) => ProtoKindSubstitutable (Environment k) where
  protoOapplyKinds = mapEnvironment . protoOapplyKinds
  protoOreplaceVariables = mapEnvironment protoOreplaceVariables

instance (Ord k, ProtoKindSubstitutable k) => ProtoKindSubstitutable (Set k) where
  protoOapplyKinds = Set.map . protoOapplyKinds
  protoOreplaceVariables = Set.map protoOreplaceVariables

instance (ProtoKindSubstitutable n, ProtoKindSubstitutable k) => ProtoKindSubstitutable (n, k) where
  protoOapplyKinds sub (a, b) = (protoOapplyKinds sub a, protoOapplyKinds sub b)
  protoOreplaceVariables = fmap protoOreplaceVariables

instance (Ord (o k), ProtoKindSubstitutable (o k), ProtoKindSubstitutable t) => ProtoKindSubstitutable (Scheme o k t) where
  protoOapplyKinds sub =
    \case
      Forall{..} ->
        Forall
          { schemeTypeVariables = protoOapplyKinds sub schemeTypeVariables
          , schemeTraits = protoOapplyKinds sub schemeTraits
          , schemeTypeBody = protoOapplyKinds sub schemeTypeBody
          }
  protoOreplaceVariables =
    \case
      Forall{..} ->
        Forall
          { schemeTypeVariables = protoOreplaceVariables schemeTypeVariables
          , schemeTraits = protoOreplaceVariables schemeTraits
          , schemeTypeBody = protoOreplaceVariables schemeTypeBody
          }

instance (ProtoKindSubstitutable k) => ProtoKindSubstitutable (Trait k) where
  protoOapplyKinds = fmap . protoOapplyKinds
  protoOreplaceVariables = fmap protoOreplaceVariables

instance ProtoKindSubstitutable (Map Int Kind) where
  protoOapplyKinds = fmap . protoOapplyKinds
  protoOreplaceVariables = fmap protoOreplaceVariables

instance ProtoKindSubstitutable ProtoKindConstraint where
  protoOapplyKinds sub =
    \case
      ProtoKEquality k1 k2 ->
        ProtoKEquality (protoOapplyKinds sub k1) (protoOapplyKinds sub k2)
  protoOreplaceVariables =
    \case
      ProtoKEquality k1 k2 ->
        ProtoKEquality (protoOreplaceVariables k1) (protoOreplaceVariables k2)

instance ProtoKindSubstitutable Kind where
  protoOapplyKinds sub =
    \case
      KArrow k1 k2 ->
        KArrow (protoOapplyKinds sub k1) (protoOapplyKinds sub k2)
      KVariable n ->
        fromMaybe (KVariable n) (Map.lookup n (kindSubstitutionMap sub))
      k ->
        k
  protoOreplaceVariables =
    \case
      KArrow k1 k2 ->
        KArrow (protoOreplaceVariables k1) (protoOreplaceVariables k2)
      KVariable{} ->
        KType
      k ->
        k

instance (ProtoKindSubstitutable k) => ProtoKindSubstitutable (Parameter k) where
  protoOapplyKinds sub =
    \case
      Parameter k name ->
        Parameter (protoOapplyKinds sub k) name
  protoOreplaceVariables =
    \case
      Parameter k name ->
        Parameter (protoOreplaceVariables k) name

instance (ProtoKindSubstitutable k) => ProtoKindSubstitutable (TypeIndex k) where
  protoOapplyKinds sub =
    \case
      TypeIndex{..} ->
        TypeIndex
          { typeIndexKind = protoOapplyKinds sub typeIndexKind
          , ..
          }
  protoOreplaceVariables =
    \case
      TypeIndex{..} ->
        TypeIndex
          { typeIndexKind = protoOreplaceVariables typeIndexKind
          , ..
          }

instance (ProtoKindSubstitutable (o Kind)) => ProtoKindSubstitutable (Type o Kind) where
  protoOapplyKinds sub =
    \case
      TApplication k t1 t2 ->
        TApplication (protoOapplyKinds sub k) (protoOapplyKinds sub t1) (protoOapplyKinds sub t2)
      TArrow t1 t2 ->
        TArrow (protoOapplyKinds sub t1) (protoOapplyKinds sub t2)
      TConstructor k name ->
        TConstructor (protoOapplyKinds sub k) name
      TIntrinsic i ->
        TIntrinsic i
      TRecord t ->
        TRecord (protoOapplyKinds sub t)
      TRow row ->
        TRow (protoOapplyKinds sub row)
      TVariable param ->
        TVariable (protoOapplyKinds sub param)
      TAlias name ts t ->
        TAlias name (fmap (protoOapplyKinds sub) ts) (protoOapplyKinds sub t)
  protoOreplaceVariables =
    \case
      TApplication k t1 t2 ->
        TApplication (protoOreplaceVariables k) (protoOreplaceVariables t1) (protoOreplaceVariables t2)
      TArrow t1 t2 ->
        TArrow (protoOreplaceVariables t1) (protoOreplaceVariables t2)
      TConstructor k name ->
        TConstructor (protoOreplaceVariables k) name
      TIntrinsic i ->
        TIntrinsic i
      TRecord t ->
        TRecord (protoOreplaceVariables t)
      TRow row ->
        TRow (protoOreplaceVariables row)
      TVariable var ->
        TVariable (protoOreplaceVariables var)
      TAlias name ts t ->
        TAlias name (fmap protoOreplaceVariables ts) (protoOreplaceVariables t)

instance (ProtoKindSubstitutable n, ProtoKindSubstitutable (o n), ProtoKindSubstitutable k) => ProtoKindSubstitutable (Row o n k) where
  protoOapplyKinds sub =
    \case
      RExtend name t row ->
        RExtend name (protoOapplyKinds sub t) (protoOapplyKinds sub row)
      RVariable var -> do
        RVariable (protoOapplyKinds sub var)
      RNil ->
        RNil
  protoOreplaceVariables =
    \case
      RExtend name t row ->
        RExtend name (protoOreplaceVariables t) (protoOreplaceVariables row)
      RVariable var -> do
        RVariable (protoOreplaceVariables var)
      RNil ->
        RNil

instance (ProtoKindSubstitutable k, ProtoKindSubstitutable t, Ord k) => ProtoKindSubstitutable (DataConstructor Parameter k t) where
  protoOapplyKinds sub DataConstructor{..} =
    DataConstructor{constructorScheme = protoOapplyKinds sub constructorScheme, ..}
  protoOreplaceVariables DataConstructor{..} =
    DataConstructor{constructorScheme = protoOreplaceVariables constructorScheme, ..}

instance ProtoKindSubstitutable (DataConstructor TypeIndex Kind (Type TypeIndex Kind)) where
  protoOapplyKinds sub DataConstructor{..} =
    DataConstructor{constructorScheme = protoOapplyKinds sub constructorScheme, ..}
  protoOreplaceVariables DataConstructor{..} =
    DataConstructor{constructorScheme = protoOreplaceVariables constructorScheme, ..}

instance ProtoKindSubstitutable (ProtoModule a Kind ()) where
  protoOapplyKinds sub ProtoModule{..} =
    ProtoModule
      { protoOmoduleDefinitions = protoOapplyKinds sub protoOmoduleDefinitions
      , ..
      }
  protoOreplaceVariables ProtoModule{..} =
    ProtoModule
      { protoOmoduleDefinitions = protoOreplaceVariables protoOmoduleDefinitions
      , ..
      }

instance ProtoKindSubstitutable (ProtoDefinition a Kind ()) where
  protoOapplyKinds sub =
    \case
      ProtoDType a name def ->
        ProtoDType a name (protoOapplyKinds sub def)
      ProtoDTypeAlias a name def ->
        ProtoDTypeAlias a name (protoOapplyKinds sub def)
      ProtoDFunction a name def ->
        ProtoDFunction a name (protoOapplyKinds sub def)
      ProtoDFunctionGroup a name defs ->
        ProtoDFunctionGroup a name (protoOapplyKinds sub <$> defs)
      ProtoDFold a name def ->
        ProtoDFold a name (protoOapplyKinds sub def)
      ProtoDLet a name def ->
        ProtoDLet a name (protoOapplyKinds sub def)
      def@ProtoDImport{} ->
        def
      def@ProtoDQualifiedImport{} ->
        def
      ProtoDTrait a name def ->
        ProtoDTrait a name (protoOapplyKinds sub def)
      ProtoDInstance a def ->
        ProtoDInstance a (protoOapplyKinds sub def)
  protoOreplaceVariables =
    \case
      ProtoDType a name def ->
        ProtoDType a name (protoOreplaceVariables def)
      ProtoDTypeAlias a name def ->
        ProtoDTypeAlias a name (protoOreplaceVariables def)
      ProtoDFunction a name def ->
        ProtoDFunction a name (protoOreplaceVariables def)
      ProtoDFunctionGroup a name defs ->
        ProtoDFunctionGroup a name (protoOreplaceVariables <$> defs)
      ProtoDFold a name def ->
        ProtoDFold a name (protoOreplaceVariables def)
      ProtoDLet a name def ->
        ProtoDLet a name (protoOreplaceVariables def)
      def@ProtoDImport{} ->
        def
      def@ProtoDQualifiedImport{} ->
        def
      ProtoDTrait a name def ->
        ProtoDTrait a name (protoOreplaceVariables def)
      ProtoDInstance a def ->
        ProtoDInstance a (protoOreplaceVariables def)

instance ProtoKindSubstitutable (Expression a Kind ()) where
  protoOapplyKinds sub =
    \case
      EAnnotation a t e ->
        EAnnotation a t (protoOapplyKinds sub e)
      EApplication a () e es ->
        EApplication a () (protoOapplyKinds sub e) (protoOapplyKinds sub es)
      ELambda a ps e ->
        ELambda a (protoOapplyKinds sub ps) (protoOapplyKinds sub e)
      ELet a bs e ->
        ELet a (protoOapplyKinds sub bs) (protoOapplyKinds sub e)
      ERecursiveLet a p e1 e2 ->
        ERecursiveLet a (protoOapplyKinds sub p) (protoOapplyKinds sub e1) (protoOapplyKinds sub e2)
      EIf a () e1 e2 e3 ->
        EIf a () (protoOapplyKinds sub e1) (protoOapplyKinds sub e2) (protoOapplyKinds sub e3)
      ERecord a () d e ->
        ERecord a () (protoOapplyKinds sub d) (protoOapplyKinds sub e)
      EListCons a () e1 e2 ->
        EListCons a () (protoOapplyKinds sub e1) (protoOapplyKinds sub e2)
      EListLiteral a () es ->
        EListLiteral a () (protoOapplyKinds sub es)
      ETuple a () es ->
        ETuple a () (protoOapplyKinds sub es)
      EMatch a () e cs ->
        EMatch a () (protoOapplyKinds sub e) (protoOapplyKinds sub cs)
      ELambdaMatch a () cs ->
        ELambdaMatch a () (protoOapplyKinds sub cs)
      ECompiledMatch a () e cs ->
        ECompiledMatch a () (protoOapplyKinds sub e) (protoOapplyKinds sub cs)
      EFold a () es cs ->
        EFold a () (protoOapplyKinds sub es) (protoOapplyKinds sub cs)
      ESelect a ll e ->
        ESelect a ll (protoOapplyKinds sub e)
      EFocus a name ll1 ll2 e1 e2 ->
        EFocus a name ll1 ll2 (protoOapplyKinds sub e1) (protoOapplyKinds sub e2)
      EFFICall a () ll es e ->
        EFFICall a () ll (protoOapplyKinds sub es) (protoOapplyKinds sub e)
      EDoBlock a is ->
        EDoBlock a (protoOapplyKinds sub is)
      e ->
        e
  protoOreplaceVariables =
    \case
      EAnnotation a t e ->
        EAnnotation a t (protoOreplaceVariables e)
      EApplication a () e es ->
        EApplication a () (protoOreplaceVariables e) (protoOreplaceVariables es)
      ELambda a ps e ->
        ELambda a (protoOreplaceVariables ps) (protoOreplaceVariables e)
      ELet a bs e ->
        ELet a (protoOreplaceVariables bs) (protoOreplaceVariables e)
      ERecursiveLet a p e1 e2 ->
        ERecursiveLet a (protoOreplaceVariables p) (protoOreplaceVariables e1) (protoOreplaceVariables e2)
      EIf a () e1 e2 e3 ->
        EIf a () (protoOreplaceVariables e1) (protoOreplaceVariables e2) (protoOreplaceVariables e3)
      ERecord a () d e ->
        ERecord a () (protoOreplaceVariables d) (protoOreplaceVariables e)
      EListCons a () e1 e2 ->
        EListCons a () (protoOreplaceVariables e1) (protoOreplaceVariables e2)
      EListLiteral a () es ->
        EListLiteral a () (protoOreplaceVariables es)
      ETuple a () es ->
        ETuple a () (protoOreplaceVariables es)
      EMatch a () e cs ->
        EMatch a () (protoOreplaceVariables e) (protoOreplaceVariables cs)
      ELambdaMatch a () cs ->
        ELambdaMatch a () (protoOreplaceVariables cs)
      ECompiledMatch a () e cs ->
        ECompiledMatch a () (protoOreplaceVariables e) (protoOreplaceVariables cs)
      EFold a () es cs ->
        EFold a () (protoOreplaceVariables es) (protoOreplaceVariables cs)
      ESelect a ll e ->
        ESelect a ll (protoOreplaceVariables e)
      EFocus a name ll1 ll2 e1 e2 ->
        EFocus a name ll1 ll2 (protoOreplaceVariables e1) (protoOreplaceVariables e2)
      EFFICall a () ll es e ->
        EFFICall a () ll (protoOreplaceVariables es) (protoOreplaceVariables e)
      EDoBlock a is ->
        EDoBlock a (protoOreplaceVariables is)
      e ->
        e

instance ProtoKindSubstitutable (Binding Expression a Kind ()) where
  protoOapplyKinds sub =
    \case
      BPattern a p e ->
        BPattern a (protoOapplyKinds sub p) (protoOapplyKinds sub e)
      BFunction a name ps e ->
        BFunction a name (protoOapplyKinds sub ps) (protoOapplyKinds sub e)
  protoOreplaceVariables =
    \case
      BPattern a p e ->
        BPattern a (protoOreplaceVariables p) (protoOreplaceVariables e)
      BFunction a name ps e ->
        BFunction a name (protoOreplaceVariables ps) (protoOreplaceVariables e)

instance ProtoKindSubstitutable (Pattern a Kind ()) where
  protoOapplyKinds sub =
    \case
      PAnnotation a t p ->
        PAnnotation a (protoOapplyKinds sub t) (protoOapplyKinds sub p)
      PConstructor a t ps ->
        PConstructor a t (protoOapplyKinds sub ps)
      PRecord a () d p ->
        PRecord a () (protoOapplyKinds sub d) (protoOapplyKinds sub p)
      PListCons a () p1 p2 ->
        PListCons a () (protoOapplyKinds sub p1) (protoOapplyKinds sub p2)
      PListLiteral a () ps ->
        PListLiteral a () (protoOapplyKinds sub ps)
      PTuple a () ps ->
        PTuple a () (protoOapplyKinds sub ps)
      POr a () p1 p2 ->
        POr a () (protoOapplyKinds sub p1) (protoOapplyKinds sub p2)
      PAs a t p ->
        PAs a t (protoOapplyKinds sub p)
      p ->
        p
  protoOreplaceVariables =
    \case
      PAnnotation a t p ->
        PAnnotation a (protoOreplaceVariables t) (protoOreplaceVariables p)
      PConstructor a t ps ->
        PConstructor a t (protoOreplaceVariables ps)
      PRecord a () d p ->
        PRecord a () (protoOreplaceVariables d) (protoOreplaceVariables p)
      PListCons a () p1 p2 ->
        PListCons a () (protoOreplaceVariables p1) (protoOreplaceVariables p2)
      PListLiteral a () ps ->
        PListLiteral a () (protoOreplaceVariables ps)
      PTuple a () ps ->
        PTuple a () (protoOreplaceVariables ps)
      POr a () p1 p2 ->
        POr a () (protoOreplaceVariables p1) (protoOreplaceVariables p2)
      PAs a t p ->
        PAs a t (protoOreplaceVariables p)
      p ->
        p

instance ProtoKindSubstitutable (ProtoTypeDefinition a Kind ()) where
  protoOapplyKinds sub ProtoTypeDefinition{..} =
    ProtoTypeDefinition
      { protoOtypeDefinitionParameters = protoOapplyKinds sub protoOtypeDefinitionParameters
      , protoOtypeDefinitionConstructors = protoOapplyKinds sub protoOtypeDefinitionConstructors
      }
  protoOreplaceVariables ProtoTypeDefinition{..} =
    ProtoTypeDefinition
      { protoOtypeDefinitionParameters = protoOreplaceVariables protoOtypeDefinitionParameters
      , protoOtypeDefinitionConstructors = protoOreplaceVariables protoOtypeDefinitionConstructors
      }

instance ProtoKindSubstitutable (ProtoFunctionDefinition a Kind ()) where
  protoOapplyKinds sub ProtoFunctionDefinition{..} =
    ProtoFunctionDefinition
      { protoOfunctionDefinitionAnnotation = protoOapplyKinds sub protoOfunctionDefinitionAnnotation
      , protoOfunctionDefinitionPatterns = protoOapplyKinds sub protoOfunctionDefinitionPatterns
      , protoOfunctionDefinitionExpression = protoOapplyKinds sub protoOfunctionDefinitionExpression
      , ..
      }
  protoOreplaceVariables ProtoFunctionDefinition{..} =
    ProtoFunctionDefinition
      { protoOfunctionDefinitionAnnotation = protoOreplaceVariables protoOfunctionDefinitionAnnotation
      , protoOfunctionDefinitionPatterns = protoOreplaceVariables protoOfunctionDefinitionPatterns
      , protoOfunctionDefinitionExpression = protoOreplaceVariables protoOfunctionDefinitionExpression
      , ..
      }

instance ProtoKindSubstitutable (ProtoLetDefinition a Kind ()) where
  protoOapplyKinds sub ProtoLetDefinition{..} =
    ProtoLetDefinition
      { protoOletDefinitionAnnotation = protoOapplyKinds sub protoOletDefinitionAnnotation
      , protoOletDefinitionExpression = protoOapplyKinds sub protoOletDefinitionExpression
      , ..
      }
  protoOreplaceVariables ProtoLetDefinition{..} =
    ProtoLetDefinition
      { protoOletDefinitionAnnotation = protoOreplaceVariables protoOletDefinitionAnnotation
      , protoOletDefinitionExpression = protoOreplaceVariables protoOletDefinitionExpression
      , ..
      }

instance ProtoKindSubstitutable (ProtoTraitDefinition a Kind) where
  protoOapplyKinds sub ProtoTraitDefinition{..} =
    ProtoTraitDefinition
      { protoOtraitDefinitionConstraints = protoOapplyKinds sub protoOtraitDefinitionConstraints
      , protoOtraitDefinitionParameter = protoOapplyKinds sub protoOtraitDefinitionParameter
      , protoOtraitDefinitionInterface = protoOapplyKinds sub protoOtraitDefinitionInterface
      , ..
      }
  protoOreplaceVariables ProtoTraitDefinition{..} =
    ProtoTraitDefinition
      { protoOtraitDefinitionConstraints = protoOreplaceVariables protoOtraitDefinitionConstraints
      , protoOtraitDefinitionParameter = protoOreplaceVariables protoOtraitDefinitionParameter
      , protoOtraitDefinitionInterface = protoOreplaceVariables protoOtraitDefinitionInterface
      , ..
      }

instance ProtoKindSubstitutable (ProtoTraitDefinitionInterfaceEntry Kind) where
  protoOapplyKinds sub ProtoTraitDefinitionInterfaceEntry{..} =
    ProtoTraitDefinitionInterfaceEntry
      protoOtraitDefinitionInterfaceEntryName
      (protoOapplyKinds sub protoOtraitDefinitionInterfaceEntryScheme)
  protoOreplaceVariables ProtoTraitDefinitionInterfaceEntry{..} =
    ProtoTraitDefinitionInterfaceEntry
      protoOtraitDefinitionInterfaceEntryName
      (protoOreplaceVariables protoOtraitDefinitionInterfaceEntryScheme)

instance ProtoKindSubstitutable (ProtoInstanceDefinition a Kind ()) where
  protoOapplyKinds sub ProtoInstanceDefinition{..} =
    ProtoInstanceDefinition
      { protoOinstanceDefinitionConstraints = protoOapplyKinds sub protoOinstanceDefinitionConstraints
      , protoOinstanceDefinitionType = protoOapplyKinds sub protoOinstanceDefinitionType
      , protoOinstanceDefinitionImplementations = protoOapplyKinds sub protoOinstanceDefinitionImplementations
      , ..
      }
  protoOreplaceVariables ProtoInstanceDefinition{..} =
    ProtoInstanceDefinition
      { protoOinstanceDefinitionConstraints = protoOreplaceVariables protoOinstanceDefinitionConstraints
      , protoOinstanceDefinitionType = protoOreplaceVariables protoOinstanceDefinitionType
      , protoOinstanceDefinitionImplementations = protoOreplaceVariables protoOinstanceDefinitionImplementations
      , ..
      }

instance ProtoKindSubstitutable (ProtoFoldDefinition a Kind ()) where
  protoOapplyKinds sub ProtoFoldDefinition{..} =
    ProtoFoldDefinition
      { protoOfoldDefinitionAnnotation = protoOapplyKinds sub protoOfoldDefinitionAnnotation
      , protoOfoldDefinitionClauses = protoOapplyKinds sub protoOfoldDefinitionClauses
      , ..
      }
  protoOreplaceVariables ProtoFoldDefinition{..} =
    ProtoFoldDefinition
      { protoOfoldDefinitionAnnotation = protoOreplaceVariables protoOfoldDefinitionAnnotation
      , protoOfoldDefinitionClauses = protoOreplaceVariables protoOfoldDefinitionClauses
      , ..
      }

instance ProtoKindSubstitutable (ProtoAliasDefinition a Kind) where
  protoOapplyKinds sub ProtoAliasDefinition{..} =
    ProtoAliasDefinition
      { protoOaliasDefinitionParameters = protoOapplyKinds sub protoOaliasDefinitionParameters
      , protoOaliasDefinitionType = protoOapplyKinds sub protoOaliasDefinitionType
      }
  protoOreplaceVariables ProtoAliasDefinition{..} =
    ProtoAliasDefinition
      { protoOaliasDefinitionParameters = protoOreplaceVariables protoOaliasDefinitionParameters
      , protoOaliasDefinitionType = protoOreplaceVariables protoOaliasDefinitionType
      }

instance ProtoKindSubstitutable (Clause a Kind ()) where
  protoOapplyKinds sub =
    \case
      EClause a p cs ->
        EClause a (protoOapplyKinds sub p) (protoOapplyKinds sub cs)
  protoOreplaceVariables =
    \case
      EClause a p cs ->
        EClause a (protoOreplaceVariables p) (protoOreplaceVariables cs)

instance ProtoKindSubstitutable (CompiledClause a Kind ()) where
  protoOapplyKinds sub =
    \case
      ECompiledClause a lls e ->
        ECompiledClause a lls (protoOapplyKinds sub e)
  protoOreplaceVariables =
    \case
      ECompiledClause a lls e ->
        ECompiledClause a lls (protoOreplaceVariables e)

instance ProtoKindSubstitutable (Choice Expression a Kind ()) where
  protoOapplyKinds sub =
    \case
      CPlain a gs e ->
        CPlain a (protoOapplyKinds sub gs) (protoOapplyKinds sub e)
  protoOreplaceVariables =
    \case
      CPlain a gs e ->
        CPlain a (protoOreplaceVariables gs) (protoOreplaceVariables e)

instance ProtoKindSubstitutable (Guard Expression a Kind ()) where
  protoOapplyKinds sub =
    \case
      CGuard e ->
        CGuard (protoOapplyKinds sub e)
  protoOreplaceVariables =
    \case
      CGuard e ->
        CGuard (protoOreplaceVariables e)

instance ProtoKindSubstitutable (With (Type Parameter Kind)) where
  protoOapplyKinds sub =
    \case
      With ts t ->
        With (protoOapplyKinds sub ts) (protoOapplyKinds sub t)
  protoOreplaceVariables =
    \case
      With ts t ->
        With (protoOreplaceVariables ts) (protoOreplaceVariables t)

instance ProtoKindSubstitutable (ProtoBuild a) where
  protoOapplyKinds sub =
    \case
      ProtoBuild{..} ->
        ProtoBuild
          { protoObuildNames = protoOapplyKinds sub protoObuildNames
          , protoObuildDataConstructors = protoOapplyKinds sub protoObuildDataConstructors
          , protoObuildTypeConstructors = protoOapplyKinds sub protoObuildTypeConstructors
          , protoObuildTraits = protoOapplyKinds sub protoObuildTraits
          , protoObuildInstances = mapEnvironment (protoOapplyKinds sub) protoObuildInstances
          , protoObuildAliases = protoOapplyKinds sub protoObuildAliases
          , ..
          }
  protoOreplaceVariables =
    \case
      ProtoBuild{..} ->
        ProtoBuild
          { protoObuildNames = protoOreplaceVariables protoObuildNames
          , protoObuildDataConstructors = protoOreplaceVariables protoObuildDataConstructors
          , protoObuildTypeConstructors = protoOreplaceVariables protoObuildTypeConstructors
          , protoObuildTraits = protoOreplaceVariables protoObuildTraits
          , protoObuildInstances = mapEnvironment protoOreplaceVariables protoObuildInstances
          , protoObuildAliases = protoOreplaceVariables protoObuildAliases
          , ..
          }

instance (ProtoKindSubstitutable a) => ProtoKindSubstitutable (InstanceMap a) where
  protoOapplyKinds = Map.map . protoOapplyKinds
  protoOreplaceVariables = Map.map protoOreplaceVariables

instance ProtoKindSubstitutable (ProtoDataConstructorEntry a) where
  protoOapplyKinds sub =
    \case
      ProtoDataConstructorEntry{..} ->
        ProtoDataConstructorEntry
          { protoOdataConstructorEntryConstructor =
              protoOapplyKinds sub protoOdataConstructorEntryConstructor
          , ..
          }
  protoOreplaceVariables =
    \case
      ProtoDataConstructorEntry{..} ->
        ProtoDataConstructorEntry
          { protoOdataConstructorEntryConstructor =
              protoOreplaceVariables protoOdataConstructorEntryConstructor
          , ..
          }

instance ProtoKindSubstitutable (ProtoTypeConstructorEntry a) where
  protoOapplyKinds sub =
    \case
      ProtoTypeConstructorEntry{..} ->
        ProtoTypeConstructorEntry
          { protoOtypeConstructorEntryKind =
              protoOapplyKinds sub protoOtypeConstructorEntryKind
          , ..
          }
  protoOreplaceVariables =
    \case
      ProtoTypeConstructorEntry{..} ->
        ProtoTypeConstructorEntry
          { protoOtypeConstructorEntryKind =
              protoOreplaceVariables protoOtypeConstructorEntryKind
          , ..
          }

instance ProtoKindSubstitutable (ProtoTraitEntry a) where
  protoOapplyKinds sub =
    \case
      ProtoTraitEntry{..} ->
        ProtoTraitEntry
          { protoOtraitEntryParameter = protoOapplyKinds sub protoOtraitEntryParameter
          , protoOtraitEntryConstraints = protoOapplyKinds sub protoOtraitEntryConstraints
          , protoOtraitEntryInterface = protoOapplyKinds sub protoOtraitEntryInterface
          , ..
          }
  protoOreplaceVariables =
    \case
      ProtoTraitEntry{..} ->
        ProtoTraitEntry
          { protoOtraitEntryParameter = protoOreplaceVariables protoOtraitEntryParameter
          , protoOtraitEntryConstraints = protoOreplaceVariables protoOtraitEntryConstraints
          , protoOtraitEntryInterface = protoOreplaceVariables protoOtraitEntryInterface
          , ..
          }

instance ProtoKindSubstitutable (ProtoInstanceEntry a) where
  protoOapplyKinds sub =
    \case
      ProtoInstanceEntry{..} ->
        ProtoInstanceEntry
          { protoOinstanceEntryType = protoOapplyKinds sub protoOinstanceEntryType
          , protoOinstanceEntryIndexedType = protoOapplyKinds sub protoOinstanceEntryIndexedType
          , protoOinstanceEntryTypeSchemes = protoOapplyKinds sub protoOinstanceEntryTypeSchemes
          , ..
          }
  protoOreplaceVariables =
    \case
      ProtoInstanceEntry{..} ->
        ProtoInstanceEntry
          { protoOinstanceEntryType = protoOreplaceVariables protoOinstanceEntryType
          , protoOinstanceEntryIndexedType = protoOreplaceVariables protoOinstanceEntryIndexedType
          , protoOinstanceEntryTypeSchemes = protoOreplaceVariables protoOinstanceEntryTypeSchemes
          , ..
          }

instance ProtoKindSubstitutable (ProtoAliasEntry a) where
  protoOapplyKinds sub =
    \case
      ProtoAliasEntry{..} ->
        ProtoAliasEntry
          { protoOaliasEntryType = protoOapplyKinds sub protoOaliasEntryType
          , ..
          }
  protoOreplaceVariables =
    \case
      ProtoAliasEntry{..} ->
        ProtoAliasEntry
          { protoOaliasEntryType = protoOreplaceVariables protoOaliasEntryType
          , ..
          }

instance ProtoKindSubstitutable ProtoNameEntry where
  protoOapplyKinds sub =
    \case
      ProtoNName n s ->
        ProtoNName n (protoOapplyKinds sub s)
      entry ->
        entry
  protoOreplaceVariables =
    \case
      ProtoNName n s ->
        ProtoNName n (protoOreplaceVariables s)
      entry ->
        entry
