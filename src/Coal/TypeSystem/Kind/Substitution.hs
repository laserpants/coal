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
import Coal.Language.Module
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
    s3 = protoOapplyKinds s1 (kindSubstitutionMap s2)

instance Monoid KindSubstitution where
  mempty = KindSubstitution mempty

class KindSubstitutable k where
  protoOapplyKinds :: KindSubstitution -> k -> k
  protoOreplaceVariables :: k -> k

instance (KindSubstitutable k) => KindSubstitutable [k] where
  protoOapplyKinds = fmap . protoOapplyKinds
  protoOreplaceVariables = fmap protoOreplaceVariables

instance (KindSubstitutable k) => KindSubstitutable (NonEmpty k) where
  protoOapplyKinds = fmap . protoOapplyKinds
  protoOreplaceVariables = fmap protoOreplaceVariables

instance (KindSubstitutable k) => KindSubstitutable (Maybe k) where
  protoOapplyKinds = fmap . protoOapplyKinds
  protoOreplaceVariables = fmap protoOreplaceVariables

instance (KindSubstitutable k) => KindSubstitutable (Map Name k) where
  protoOapplyKinds = fmap . protoOapplyKinds
  protoOreplaceVariables = fmap protoOreplaceVariables

instance (KindSubstitutable k) => KindSubstitutable (Environment k) where
  protoOapplyKinds = mapEnvironment . protoOapplyKinds
  protoOreplaceVariables = mapEnvironment protoOreplaceVariables

instance (Ord k, KindSubstitutable k) => KindSubstitutable (Set k) where
  protoOapplyKinds = Set.map . protoOapplyKinds
  protoOreplaceVariables = Set.map protoOreplaceVariables

instance (KindSubstitutable n, KindSubstitutable k) => KindSubstitutable (n, k) where
  protoOapplyKinds sub (a, b) = (protoOapplyKinds sub a, protoOapplyKinds sub b)
  protoOreplaceVariables = fmap protoOreplaceVariables

instance (Ord (o k), Ord t, KindSubstitutable (o k), KindSubstitutable t) => KindSubstitutable (Scheme o k t) where
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

instance (KindSubstitutable k) => KindSubstitutable (Trait k) where
  protoOapplyKinds = fmap . protoOapplyKinds
  protoOreplaceVariables = fmap protoOreplaceVariables

instance KindSubstitutable (Map Int Kind) where
  protoOapplyKinds = fmap . protoOapplyKinds
  protoOreplaceVariables = fmap protoOreplaceVariables

instance KindSubstitutable KindConstraint where
  protoOapplyKinds sub =
    \case
      KEquality k1 k2 ->
        KEquality (protoOapplyKinds sub k1) (protoOapplyKinds sub k2)
  protoOreplaceVariables =
    \case
      KEquality k1 k2 ->
        KEquality (protoOreplaceVariables k1) (protoOreplaceVariables k2)

instance KindSubstitutable Kind where
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

instance (KindSubstitutable k) => KindSubstitutable (Parameter k) where
  protoOapplyKinds sub =
    \case
      Parameter k name ->
        Parameter (protoOapplyKinds sub k) name
  protoOreplaceVariables =
    \case
      Parameter k name ->
        Parameter (protoOreplaceVariables k) name

instance (KindSubstitutable k) => KindSubstitutable (TypeIndex k) where
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

instance (KindSubstitutable (o Kind)) => KindSubstitutable (Type o Kind) where
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

instance (KindSubstitutable n, KindSubstitutable (o n), KindSubstitutable k) => KindSubstitutable (Row o n k) where
  protoOapplyKinds sub =
    \case
      RExtend name t row ->
        RExtend name (protoOapplyKinds sub t) (protoOapplyKinds sub row)
      RVariable var ->
        RVariable (protoOapplyKinds sub var)
      RNil ->
        RNil
  protoOreplaceVariables =
    \case
      RExtend name t row ->
        RExtend name (protoOreplaceVariables t) (protoOreplaceVariables row)
      RVariable var ->
        RVariable (protoOreplaceVariables var)
      RNil ->
        RNil

instance (KindSubstitutable k, KindSubstitutable t, Ord k, Ord t) => KindSubstitutable (DataConstructor Parameter k t) where
  protoOapplyKinds sub DataConstructor{..} =
    DataConstructor{constructorScheme = protoOapplyKinds sub constructorScheme, ..}
  protoOreplaceVariables DataConstructor{..} =
    DataConstructor{constructorScheme = protoOreplaceVariables constructorScheme, ..}

instance KindSubstitutable (DataConstructor TypeIndex Kind (Type TypeIndex Kind)) where
  protoOapplyKinds sub DataConstructor{..} =
    DataConstructor{constructorScheme = protoOapplyKinds sub constructorScheme, ..}
  protoOreplaceVariables DataConstructor{..} =
    DataConstructor{constructorScheme = protoOreplaceVariables constructorScheme, ..}

instance KindSubstitutable (Module a Kind ()) where
  protoOapplyKinds sub Module{..} =
    Module
      { protoOmoduleDefinitions = protoOapplyKinds sub protoOmoduleDefinitions
      , ..
      }
  protoOreplaceVariables Module{..} =
    Module
      { protoOmoduleDefinitions = protoOreplaceVariables protoOmoduleDefinitions
      , ..
      }

instance KindSubstitutable (Definition a Kind ()) where
  protoOapplyKinds sub =
    \case
      DType a name def ->
        DType a name (protoOapplyKinds sub def)
      DTypeAlias a name def ->
        DTypeAlias a name (protoOapplyKinds sub def)
      DFunction a name def ->
        DFunction a name (protoOapplyKinds sub def)
      DFunctionGroup a name defs ->
        DFunctionGroup a name (protoOapplyKinds sub <$> defs)
      DFold a name def ->
        DFold a name (protoOapplyKinds sub def)
      DLet a name def ->
        DLet a name (protoOapplyKinds sub def)
      def@DImport{} ->
        def
      def@DNamespaceImport{} ->
        def
      DTrait a name def ->
        DTrait a name (protoOapplyKinds sub def)
      DInstance a def ->
        DInstance a (protoOapplyKinds sub def)
  protoOreplaceVariables =
    \case
      DType a name def ->
        DType a name (protoOreplaceVariables def)
      DTypeAlias a name def ->
        DTypeAlias a name (protoOreplaceVariables def)
      DFunction a name def ->
        DFunction a name (protoOreplaceVariables def)
      DFunctionGroup a name defs ->
        DFunctionGroup a name (protoOreplaceVariables <$> defs)
      DFold a name def ->
        DFold a name (protoOreplaceVariables def)
      DLet a name def ->
        DLet a name (protoOreplaceVariables def)
      def@DImport{} ->
        def
      def@DNamespaceImport{} ->
        def
      DTrait a name def ->
        DTrait a name (protoOreplaceVariables def)
      DInstance a def ->
        DInstance a (protoOreplaceVariables def)

instance KindSubstitutable (Expression a Kind ()) where
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

instance KindSubstitutable (Binding Expression a Kind ()) where
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

instance KindSubstitutable (Pattern a Kind ()) where
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

instance KindSubstitutable (TypeDefinition a Kind ()) where
  protoOapplyKinds sub TypeDefinition{..} =
    TypeDefinition
      { protoOtypeDefinitionParameters = protoOapplyKinds sub protoOtypeDefinitionParameters
      , protoOtypeDefinitionConstructors = protoOapplyKinds sub protoOtypeDefinitionConstructors
      }
  protoOreplaceVariables TypeDefinition{..} =
    TypeDefinition
      { protoOtypeDefinitionParameters = protoOreplaceVariables protoOtypeDefinitionParameters
      , protoOtypeDefinitionConstructors = protoOreplaceVariables protoOtypeDefinitionConstructors
      }

instance KindSubstitutable (FunctionDefinition a Kind ()) where
  protoOapplyKinds sub FunctionDefinition{..} =
    FunctionDefinition
      { protoOfunctionDefinitionAnnotation = protoOapplyKinds sub protoOfunctionDefinitionAnnotation
      , protoOfunctionDefinitionPatterns = protoOapplyKinds sub protoOfunctionDefinitionPatterns
      , protoOfunctionDefinitionExpression = protoOapplyKinds sub protoOfunctionDefinitionExpression
      , ..
      }
  protoOreplaceVariables FunctionDefinition{..} =
    FunctionDefinition
      { protoOfunctionDefinitionAnnotation = protoOreplaceVariables protoOfunctionDefinitionAnnotation
      , protoOfunctionDefinitionPatterns = protoOreplaceVariables protoOfunctionDefinitionPatterns
      , protoOfunctionDefinitionExpression = protoOreplaceVariables protoOfunctionDefinitionExpression
      , ..
      }

instance KindSubstitutable (LetDefinition a Kind ()) where
  protoOapplyKinds sub LetDefinition{..} =
    LetDefinition
      { protoOletDefinitionAnnotation = protoOapplyKinds sub protoOletDefinitionAnnotation
      , protoOletDefinitionExpression = protoOapplyKinds sub protoOletDefinitionExpression
      , ..
      }
  protoOreplaceVariables LetDefinition{..} =
    LetDefinition
      { protoOletDefinitionAnnotation = protoOreplaceVariables protoOletDefinitionAnnotation
      , protoOletDefinitionExpression = protoOreplaceVariables protoOletDefinitionExpression
      , ..
      }

instance KindSubstitutable (TraitDefinition a Kind) where
  protoOapplyKinds sub TraitDefinition{..} =
    TraitDefinition
      { protoOtraitDefinitionConstraints = protoOapplyKinds sub protoOtraitDefinitionConstraints
      , protoOtraitDefinitionParameter = protoOapplyKinds sub protoOtraitDefinitionParameter
      , protoOtraitDefinitionInterface = protoOapplyKinds sub protoOtraitDefinitionInterface
      , ..
      }
  protoOreplaceVariables TraitDefinition{..} =
    TraitDefinition
      { protoOtraitDefinitionConstraints = protoOreplaceVariables protoOtraitDefinitionConstraints
      , protoOtraitDefinitionParameter = protoOreplaceVariables protoOtraitDefinitionParameter
      , protoOtraitDefinitionInterface = protoOreplaceVariables protoOtraitDefinitionInterface
      , ..
      }

instance KindSubstitutable (TraitDefinitionInterfaceEntry Kind) where
  protoOapplyKinds sub TraitDefinitionInterfaceEntry{..} =
    TraitDefinitionInterfaceEntry
      protoOtraitDefinitionInterfaceEntryName
      (protoOapplyKinds sub protoOtraitDefinitionInterfaceEntryScheme)
  protoOreplaceVariables TraitDefinitionInterfaceEntry{..} =
    TraitDefinitionInterfaceEntry
      protoOtraitDefinitionInterfaceEntryName
      (protoOreplaceVariables protoOtraitDefinitionInterfaceEntryScheme)

instance KindSubstitutable (InstanceDefinition a Kind ()) where
  protoOapplyKinds sub InstanceDefinition{..} =
    InstanceDefinition
      { protoOinstanceDefinitionConstraints = protoOapplyKinds sub protoOinstanceDefinitionConstraints
      , protoOinstanceDefinitionType = protoOapplyKinds sub protoOinstanceDefinitionType
      , protoOinstanceDefinitionImplementations = protoOapplyKinds sub protoOinstanceDefinitionImplementations
      , ..
      }
  protoOreplaceVariables InstanceDefinition{..} =
    InstanceDefinition
      { protoOinstanceDefinitionConstraints = protoOreplaceVariables protoOinstanceDefinitionConstraints
      , protoOinstanceDefinitionType = protoOreplaceVariables protoOinstanceDefinitionType
      , protoOinstanceDefinitionImplementations = protoOreplaceVariables protoOinstanceDefinitionImplementations
      , ..
      }

instance KindSubstitutable (FoldDefinition a Kind ()) where
  protoOapplyKinds sub FoldDefinition{..} =
    FoldDefinition
      { protoOfoldDefinitionAnnotation = protoOapplyKinds sub protoOfoldDefinitionAnnotation
      , protoOfoldDefinitionClauses = protoOapplyKinds sub protoOfoldDefinitionClauses
      , ..
      }
  protoOreplaceVariables FoldDefinition{..} =
    FoldDefinition
      { protoOfoldDefinitionAnnotation = protoOreplaceVariables protoOfoldDefinitionAnnotation
      , protoOfoldDefinitionClauses = protoOreplaceVariables protoOfoldDefinitionClauses
      , ..
      }

instance KindSubstitutable (AliasDefinition a Kind) where
  protoOapplyKinds sub AliasDefinition{..} =
    AliasDefinition
      { protoOaliasDefinitionParameters = protoOapplyKinds sub protoOaliasDefinitionParameters
      , protoOaliasDefinitionType = protoOapplyKinds sub protoOaliasDefinitionType
      }
  protoOreplaceVariables AliasDefinition{..} =
    AliasDefinition
      { protoOaliasDefinitionParameters = protoOreplaceVariables protoOaliasDefinitionParameters
      , protoOaliasDefinitionType = protoOreplaceVariables protoOaliasDefinitionType
      }

instance KindSubstitutable (Clause a Kind ()) where
  protoOapplyKinds sub =
    \case
      EClause a p cs ->
        EClause a (protoOapplyKinds sub p) (protoOapplyKinds sub cs)
  protoOreplaceVariables =
    \case
      EClause a p cs ->
        EClause a (protoOreplaceVariables p) (protoOreplaceVariables cs)

instance KindSubstitutable (CompiledClause a Kind ()) where
  protoOapplyKinds sub =
    \case
      ECompiledClause a lls e ->
        ECompiledClause a lls (protoOapplyKinds sub e)
  protoOreplaceVariables =
    \case
      ECompiledClause a lls e ->
        ECompiledClause a lls (protoOreplaceVariables e)

instance KindSubstitutable (Choice Expression a Kind ()) where
  protoOapplyKinds sub =
    \case
      CPlain a gs e ->
        CPlain a (protoOapplyKinds sub gs) (protoOapplyKinds sub e)
  protoOreplaceVariables =
    \case
      CPlain a gs e ->
        CPlain a (protoOreplaceVariables gs) (protoOreplaceVariables e)

instance KindSubstitutable (Guard Expression a Kind ()) where
  protoOapplyKinds sub =
    \case
      CGuard e ->
        CGuard (protoOapplyKinds sub e)
  protoOreplaceVariables =
    \case
      CGuard e ->
        CGuard (protoOreplaceVariables e)

instance KindSubstitutable (Qualified (Type Parameter Kind)) where
  protoOapplyKinds sub =
    \case
      With ts t ->
        With (protoOapplyKinds sub ts) (protoOapplyKinds sub t)
  protoOreplaceVariables =
    \case
      With ts t ->
        With (protoOreplaceVariables ts) (protoOreplaceVariables t)

instance KindSubstitutable (Build a) where
  protoOapplyKinds sub =
    \case
      Build{..} ->
        Build
          { protoObuildNames = protoOapplyKinds sub protoObuildNames
          , protoObuildDataConstructors = protoOapplyKinds sub protoObuildDataConstructors
          , protoObuildTypeConstructors = protoOapplyKinds sub protoObuildTypeConstructors
          , protoObuildTraits = protoOapplyKinds sub protoObuildTraits
          , protoObuildInstances = mapEnvironment (Map.mapKeys (protoOapplyKinds sub) . Map.map (protoOapplyKinds sub)) protoObuildInstances
          , protoObuildAliases = protoOapplyKinds sub protoObuildAliases
          , ..
          }
  protoOreplaceVariables =
    \case
      Build{..} ->
        Build
          { protoObuildNames = protoOreplaceVariables protoObuildNames
          , protoObuildDataConstructors = protoOreplaceVariables protoObuildDataConstructors
          , protoObuildTypeConstructors = protoOreplaceVariables protoObuildTypeConstructors
          , protoObuildTraits = protoOreplaceVariables protoObuildTraits
          , protoObuildInstances = mapEnvironment (Map.mapKeys protoOreplaceVariables . Map.map protoOreplaceVariables) protoObuildInstances
          , protoObuildAliases = protoOreplaceVariables protoObuildAliases
          , ..
          }

instance (KindSubstitutable a) => KindSubstitutable (InstanceMap a) where
  protoOapplyKinds = Map.map . protoOapplyKinds
  protoOreplaceVariables = Map.map protoOreplaceVariables

instance KindSubstitutable (DataConstructorEntry a) where
  protoOapplyKinds sub =
    \case
      DataConstructorEntry{..} ->
        DataConstructorEntry
          { protoOdataConstructorEntryConstructor =
              protoOapplyKinds sub protoOdataConstructorEntryConstructor
          , ..
          }
  protoOreplaceVariables =
    \case
      DataConstructorEntry{..} ->
        DataConstructorEntry
          { protoOdataConstructorEntryConstructor =
              protoOreplaceVariables protoOdataConstructorEntryConstructor
          , ..
          }

instance KindSubstitutable (TypeConstructorEntry a) where
  protoOapplyKinds sub =
    \case
      TypeConstructorEntry{..} ->
        TypeConstructorEntry
          { protoOtypeConstructorEntryKind =
              protoOapplyKinds sub protoOtypeConstructorEntryKind
          , ..
          }
  protoOreplaceVariables =
    \case
      TypeConstructorEntry{..} ->
        TypeConstructorEntry
          { protoOtypeConstructorEntryKind =
              protoOreplaceVariables protoOtypeConstructorEntryKind
          , ..
          }

instance KindSubstitutable (TraitEntry a) where
  protoOapplyKinds sub =
    \case
      TraitEntry{..} ->
        TraitEntry
          { protoOtraitEntryParameter = protoOapplyKinds sub protoOtraitEntryParameter
          , protoOtraitEntryConstraints = protoOapplyKinds sub protoOtraitEntryConstraints
          , protoOtraitEntryInterface = protoOapplyKinds sub protoOtraitEntryInterface
          , ..
          }
  protoOreplaceVariables =
    \case
      TraitEntry{..} ->
        TraitEntry
          { protoOtraitEntryParameter = protoOreplaceVariables protoOtraitEntryParameter
          , protoOtraitEntryConstraints = protoOreplaceVariables protoOtraitEntryConstraints
          , protoOtraitEntryInterface = protoOreplaceVariables protoOtraitEntryInterface
          , ..
          }

instance KindSubstitutable (InstanceEntry a) where
  protoOapplyKinds sub =
    \case
      InstanceEntry{..} ->
        InstanceEntry
          { protoOinstanceEntryType = protoOapplyKinds sub protoOinstanceEntryType
          , protoOinstanceEntryIndexedType = protoOapplyKinds sub protoOinstanceEntryIndexedType
          , protoOinstanceEntryTypeSchemes = protoOapplyKinds sub protoOinstanceEntryTypeSchemes
          , ..
          }
  protoOreplaceVariables =
    \case
      InstanceEntry{..} ->
        InstanceEntry
          { protoOinstanceEntryType = protoOreplaceVariables protoOinstanceEntryType
          , protoOinstanceEntryIndexedType = protoOreplaceVariables protoOinstanceEntryIndexedType
          , protoOinstanceEntryTypeSchemes = protoOreplaceVariables protoOinstanceEntryTypeSchemes
          , ..
          }

instance KindSubstitutable (AliasEntry a) where
  protoOapplyKinds sub =
    \case
      AliasEntry{..} ->
        AliasEntry
          { protoOaliasEntryType = protoOapplyKinds sub protoOaliasEntryType
          , protoOaliasEntryParams = protoOapplyKinds sub protoOaliasEntryParams
          , ..
          }
  protoOreplaceVariables =
    \case
      AliasEntry{..} ->
        AliasEntry
          { protoOaliasEntryType = protoOreplaceVariables protoOaliasEntryType
          , protoOaliasEntryParams = protoOreplaceVariables protoOaliasEntryParams
          , ..
          }

instance KindSubstitutable NameEntry where
  protoOapplyKinds sub =
    \case
      NName n s ->
        NName n (protoOapplyKinds sub s)
      NType n k ->
        NType n (protoOapplyKinds sub k)
      entry ->
        entry
  protoOreplaceVariables =
    \case
      NName n s ->
        NName n (protoOreplaceVariables s)
      NType n k ->
        NType n (protoOreplaceVariables k)
      entry ->
        entry
