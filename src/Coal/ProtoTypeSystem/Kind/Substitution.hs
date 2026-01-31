{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.ProtoTypeSystem.Kind.Substitution (
  ProtoKindSubstitutable (..),
  ProtoKindSubstitution (..),
) where

import Coal.Language.Data.Constructor (DataConstructor (..))
import Coal.Language.Expression (Clause (..), CompiledClause (..), Expression (..))
import Coal.Language.Expression.Binding (Binding (..))
import Coal.Language.Expression.Choice (Choice (..), Guard (..))
import Coal.Language.Pattern (Pattern (..))
import Coal.Language.Trait (Trait (..), With (..))
import Coal.Language.Type (Parameter (..), Type (..))
import Coal.Language.Type.Kind (Kind (..))
import Coal.Language.Type.Row (Row (..))
import Coal.Language.Type.Scheme (Scheme (..))
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule
import Coal.ProtoTypeSystem.Kind.Constraint (ProtoKindConstraint (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra (second)
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

instance (ProtoKindSubstitutable k) => ProtoKindSubstitutable [k] where
  protoOapplyKinds = fmap . protoOapplyKinds

instance (ProtoKindSubstitutable k) => ProtoKindSubstitutable (NonEmpty k) where
  protoOapplyKinds = fmap . protoOapplyKinds

instance (ProtoKindSubstitutable k) => ProtoKindSubstitutable (Maybe k) where
  protoOapplyKinds = fmap . protoOapplyKinds

instance (ProtoKindSubstitutable k) => ProtoKindSubstitutable (Map Name k) where
  protoOapplyKinds = fmap . protoOapplyKinds

instance (Ord k, ProtoKindSubstitutable k) => ProtoKindSubstitutable (Set k) where
  protoOapplyKinds = Set.map . protoOapplyKinds

instance (ProtoKindSubstitutable n, ProtoKindSubstitutable k) => ProtoKindSubstitutable (n, k) where
  protoOapplyKinds sub (a, b) = (protoOapplyKinds sub a, protoOapplyKinds sub b)

instance (Ord k, ProtoKindSubstitutable k, ProtoKindSubstitutable t) => ProtoKindSubstitutable (Scheme Parameter k t) where
  protoOapplyKinds sub =
    \case
      Forall{..} ->
        Forall
          { schemeTypeVariables = protoOapplyKinds sub schemeTypeVariables
          , schemeTraits = protoOapplyKinds sub schemeTraits
          , schemeTypeBody = protoOapplyKinds sub schemeTypeBody
          }

instance (ProtoKindSubstitutable k) => ProtoKindSubstitutable (Trait k) where
  protoOapplyKinds = fmap . protoOapplyKinds

instance ProtoKindSubstitutable (Map Int Kind) where
  protoOapplyKinds = fmap . protoOapplyKinds

instance ProtoKindSubstitutable ProtoKindConstraint where
  protoOapplyKinds sub =
    \case
      ProtoKEquality k1 k2 ->
        ProtoKEquality (protoOapplyKinds sub k1) (protoOapplyKinds sub k2)

instance ProtoKindSubstitutable Kind where
  protoOapplyKinds sub =
    \case
      KArrow k1 k2 ->
        KArrow (protoOapplyKinds sub k1) (protoOapplyKinds sub k2)
      KVar n ->
        fromMaybe (KVar n) (Map.lookup n (kindSubstitutionMap sub))
      k ->
        k

instance (ProtoKindSubstitutable k) => ProtoKindSubstitutable (Parameter k) where
  protoOapplyKinds sub =
    \case
      Parameter k name ->
        Parameter (protoOapplyKinds sub k) name

instance ProtoKindSubstitutable (Type Parameter Kind) where
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

instance (ProtoKindSubstitutable n, ProtoKindSubstitutable k) => ProtoKindSubstitutable (Row Parameter n k) where
  protoOapplyKinds sub =
    \case
      RExtend name t row ->
        RExtend name (protoOapplyKinds sub t) (protoOapplyKinds sub row)
      RVariable (Parameter k name) -> do
        RVariable (Parameter (protoOapplyKinds sub k) name)
      RNil ->
        RNil

instance (ProtoKindSubstitutable k, ProtoKindSubstitutable t, Ord k) => ProtoKindSubstitutable (DataConstructor Parameter k t) where
  protoOapplyKinds sub DataConstructor{..} =
    DataConstructor{constructorScheme = protoOapplyKinds sub constructorScheme, ..}

instance ProtoKindSubstitutable (ProtoModule a Kind ()) where
  protoOapplyKinds sub ProtoModule{..} =
    ProtoModule
      { protoOmoduleDefinitions = protoOapplyKinds sub protoOmoduleDefinitions
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
      EFFICall a t ll es e ->
        EFFICall a t ll (protoOapplyKinds sub es) (protoOapplyKinds sub e)
      EDoBlock a is ->
        EDoBlock a (protoOapplyKinds sub is)
      e ->
        e

instance ProtoKindSubstitutable (Binding Expression a Kind ()) where
  protoOapplyKinds sub =
    \case
      BPattern a p e ->
        BPattern a (protoOapplyKinds sub p) (protoOapplyKinds sub e)
      BFunction a name ps e ->
        BFunction a name (protoOapplyKinds sub ps) (protoOapplyKinds sub e)

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

instance ProtoKindSubstitutable (ProtoTypeDefinition a Kind ()) where
  protoOapplyKinds sub ProtoTypeDefinition{..} =
    ProtoTypeDefinition
      { protoOtypeDefinitionParameters = protoOapplyKinds sub protoOtypeDefinitionParameters
      , protoOtypeDefinitionConstructors = protoOapplyKinds sub protoOtypeDefinitionConstructors
      }

instance ProtoKindSubstitutable (ProtoFunctionDefinition a Kind ()) where
  protoOapplyKinds sub ProtoFunctionDefinition{..} =
    ProtoFunctionDefinition
      { protoOfunctionDefinitionAnnotation = protoOapplyKinds sub protoOfunctionDefinitionAnnotation
      , protoOfunctionDefinitionPatterns = protoOapplyKinds sub protoOfunctionDefinitionPatterns
      , protoOfunctionDefinitionExpression = protoOapplyKinds sub protoOfunctionDefinitionExpression
      , ..
      }

instance ProtoKindSubstitutable (ProtoLetDefinition a Kind ()) where
  protoOapplyKinds sub ProtoLetDefinition{..} =
    ProtoLetDefinition
      { protoOletDefinitionAnnotation = protoOapplyKinds sub protoOletDefinitionAnnotation
      , protoOletDefinitionExpression = protoOapplyKinds sub protoOletDefinitionExpression
      , ..
      }

instance ProtoKindSubstitutable (ProtoTraitDefinition a Kind) where
  protoOapplyKinds sub ProtoTraitDefinition{..} =
    ProtoTraitDefinition
      { protoOtraitDefinitionConstraints = protoOapplyKinds sub protoOtraitDefinitionConstraints
      , protoOtraitDefinitionParameter = protoOapplyKinds sub protoOtraitDefinitionParameter
      , protoOtraitDefinitionInterface = fmap (second (protoOapplyKinds sub)) protoOtraitDefinitionInterface
      , ..
      }

instance ProtoKindSubstitutable (ProtoInstanceDefinition a Kind ()) where
  protoOapplyKinds sub ProtoInstanceDefinition{..} =
    ProtoInstanceDefinition
      { protoOinstanceDefinitionConstraints = protoOapplyKinds sub protoOinstanceDefinitionConstraints
      , protoOinstanceDefinitionType = protoOapplyKinds sub protoOinstanceDefinitionType
      , protoOinstanceDefinitionImplementations = protoOapplyKinds sub protoOinstanceDefinitionImplementations
      , ..
      }

instance ProtoKindSubstitutable (ProtoFoldDefinition a Kind ()) where
  protoOapplyKinds sub ProtoFoldDefinition{..} =
    ProtoFoldDefinition
      { protoOfoldDefinitionAnnotation = protoOapplyKinds sub protoOfoldDefinitionAnnotation
      , protoOfoldDefinitionClauses = protoOapplyKinds sub protoOfoldDefinitionClauses
      , ..
      }

instance ProtoKindSubstitutable (ProtoAliasDefinition a Kind) where
  protoOapplyKinds sub ProtoAliasDefinition{..} =
    ProtoAliasDefinition
      { protoOaliasDefinitionParameters = protoOapplyKinds sub protoOaliasDefinitionParameters
      , protoOaliasDefinitionType = protoOapplyKinds sub protoOaliasDefinitionType
      }

instance ProtoKindSubstitutable (Clause a Kind ()) where
  protoOapplyKinds sub =
    \case
      EClause a p cs ->
        EClause a (protoOapplyKinds sub p) (protoOapplyKinds sub cs)

instance ProtoKindSubstitutable (CompiledClause a Kind ()) where
  protoOapplyKinds sub =
    \case
      ECompiledClause a lls e ->
        ECompiledClause a lls (protoOapplyKinds sub e)

instance ProtoKindSubstitutable (Choice Expression a Kind ()) where
  protoOapplyKinds sub =
    \case
      CPlain a gs e ->
        CPlain a (protoOapplyKinds sub gs) (protoOapplyKinds sub e)

instance ProtoKindSubstitutable (Guard Expression a Kind ()) where
  protoOapplyKinds sub =
    \case
      CGuard e ->
        CGuard (protoOapplyKinds sub e)

instance ProtoKindSubstitutable (With (Type Parameter Kind)) where
  protoOapplyKinds sub =
    \case
      With ts t ->
        With (protoOapplyKinds sub ts) (protoOapplyKinds sub t)
