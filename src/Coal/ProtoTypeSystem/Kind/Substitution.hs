{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.ProtoTypeSystem.Kind.Substitution (
  ProtoKindSubstitutable (..),
  ProtoKindSubstitution (..),
) where

import Coal.Language.Data.Constructor (DataConstructor (..))
import Coal.Language.Trait (Trait (..))
import Coal.Language.Type (Parameter (..), Type (..))
import Coal.Language.Type.Kind (Kind (..))
import Coal.Language.Type.Row (Row (..))
import Coal.Language.Type.Scheme (Scheme (..))
import Coal.ProtoTypeSystem.Kind.Constraint (ProtoKindConstraint (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

newtype ProtoKindSubstitution = ProtoKindSubstitution {kindSubstitutionMap :: Map Int Kind}
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

instance (Ord k, ProtoKindSubstitutable k) => ProtoKindSubstitutable (Set k) where
  protoOapplyKinds = Set.map . protoOapplyKinds

instance (ProtoKindSubstitutable n, ProtoKindSubstitutable k) => ProtoKindSubstitutable (n, k) where
  protoOapplyKinds sub (a, b) = (protoOapplyKinds sub a, protoOapplyKinds sub b)

instance (Ord k, ProtoKindSubstitutable k, ProtoKindSubstitutable t) => ProtoKindSubstitutable (Scheme Parameter k t) where
  protoOapplyKinds sub =
    \case
      Forall{..} ->
        Forall
          (protoOapplyKinds sub schemeTypeVariables)
          (protoOapplyKinds sub schemeTraits)
          (protoOapplyKinds sub schemeTypeBody)

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
