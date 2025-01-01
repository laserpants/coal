{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.TypeSubstitution (
  TypeSubstitution (..),
  TypeSubstitutable (..),
  mapsToType,
  typeSubstitutionFromList,
  normalizeTypeIndexes,
) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Noll.Label (Label (..))
import Noll.Language (
  Binding (..),
  Clause (..),
  Expression (..),
  Intrinsic (..),
  Kind (..),
  KindIndex (..),
  OpaqueRow,
  OpaqueType,
  Pattern (..),
  Row (..),
  Scheme (..),
  Trait (..),
  Type (..),
  TypeIndex (..),
  TypeIndexed (..),
 )
import Noll.Language.Expression.Choice (Choice (..), Guard (..))
import Noll.TypeSystem.TypeConstraint (MonomorphicSet (..), TypeConstraint (..))
import Noll.Utils (IndexMap, Map, Set)

class TypeSubstitutable s where
  applyTypeSub :: TypeSubstitution -> s -> s

instance (TypeSubstitutable s) => TypeSubstitutable (Map k s) where
  applyTypeSub = fmap . applyTypeSub

instance (TypeSubstitutable s) => TypeSubstitutable [s] where
  applyTypeSub = fmap . applyTypeSub

instance (TypeSubstitutable s) => TypeSubstitutable (NonEmpty s) where
  applyTypeSub = fmap . applyTypeSub

instance (TypeSubstitutable s) => TypeSubstitutable (Maybe s) where
  applyTypeSub = fmap . applyTypeSub

instance (TypeSubstitutable s) => TypeSubstitutable (Trait s) where
  applyTypeSub = fmap . applyTypeSub

instance (Ord s, TypeSubstitutable s) => TypeSubstitutable (Set s) where
  applyTypeSub = Set.map . applyTypeSub

instance TypeSubstitutable OpaqueRow where
  applyTypeSub sub =
    error "TODO"

instance TypeSubstitutable (MonomorphicSet (TypeIndex ())) where
  applyTypeSub sub =
    \case
      MonomorphicSet m ->
        MonomorphicSet (typeIndexesIn (Set.map (applyTypeSub sub . TVariable) m))

instance TypeSubstitutable (Scheme TypeIndex () OpaqueType) where
  applyTypeSub sub =
    \case
      Forall qs ps t ->
        let
          sub1 = foldr removeTypeSubstitution sub qs
         in
          Forall qs (applyTypeSub sub1 ps) (applyTypeSub sub1 t)

instance TypeSubstitutable (TypeConstraint c TypeIndex () OpaqueType) where
  applyTypeSub sub =
    \case
      Equality meta ts ->
        Equality meta (applyTypeSub sub ts)
      Implicit meta t1 t2 m ->
        Implicit meta (applyTypeSub sub t1) (applyTypeSub sub t2) (applyTypeSub sub m)
      Explicit meta t1 s ->
        Explicit meta (applyTypeSub sub t1) (applyTypeSub sub s)

instance (TypeSubstitutable s) => TypeSubstitutable (Intrinsic s) where
  applyTypeSub = fmap . applyTypeSub

instance TypeSubstitutable OpaqueType where
  applyTypeSub sub =
    \case
      TAlias name ts t -> do
        TAlias name (applyTypeSub sub ts) (applyTypeSub sub t)
      TApplication k t1 ts ->
        TApplication k (applyTypeSub sub t1) (applyTypeSub sub ts)
      TArrow t1 t2 ->
        TArrow (applyTypeSub sub t1) (applyTypeSub sub t2)
      TIntrinsic t ->
        TIntrinsic (applyTypeSub sub t)
      TRow row ->
        TRow (applyTypeSub sub row)
      TVariable t ->
        fromMaybe (TVariable t) (substitutionIndex t sub)
      t@TConstructor{} ->
        t

{-# INLINE substitutionIndex #-}
substitutionIndex :: TypeIndex () -> TypeSubstitution -> Maybe OpaqueType
substitutionIndex TypeIndex{..} sub = Map.lookup typeIndexId (typeSubstitutionMap sub)

instance TypeSubstitutable (Pattern a OpaqueType) where
  applyTypeSub sub =
    \case
      PVariable a (Label t name) ->
        PVariable a (Label (applyTypeSub sub t) name)
      PConstructor a (Label t name) ps ->
        PConstructor a (Label (applyTypeSub sub t) name) (applyTypeSub sub ps)

instance TypeSubstitutable (Binding Expression a OpaqueType) where
  applyTypeSub sub =
    \case
      BPattern a p e ->
        BPattern a (applyTypeSub sub p) (applyTypeSub sub e)

instance TypeSubstitutable (Guard Expression a OpaqueType) where
  applyTypeSub sub =
    \case
      CGuard e ->
        CGuard (applyTypeSub sub e)

instance TypeSubstitutable (Choice Expression a OpaqueType) where
  applyTypeSub sub =
    \case
      CPlain a gs e ->
        CPlain a (applyTypeSub sub gs) (applyTypeSub sub e)

instance TypeSubstitutable (Clause Expression a OpaqueType) where
  applyTypeSub sub =
    \case
      EClause a p cs ->
        EClause a (applyTypeSub sub p) (applyTypeSub sub cs)

instance TypeSubstitutable (Expression a OpaqueType) where
  applyTypeSub sub =
    \case
      EAnnotation a t e ->
        EAnnotation a t (applyTypeSub sub e)
      EConstructor a (Label t name) -> do
        EConstructor a (Label (applyTypeSub sub t) name)
      EVariable a (Label t name) -> do
        EVariable a (Label (applyTypeSub sub t) name)
      ELambda a ps e -> do
        ELambda a (applyTypeSub sub ps) (applyTypeSub sub e)
      ELet a gs e1 -> do
        ELet a (applyTypeSub sub gs) (applyTypeSub sub e1)
      EIf a t e1 e2 e3 -> do
        EIf a (applyTypeSub sub t) (applyTypeSub sub e1) (applyTypeSub sub e2) (applyTypeSub sub e3)
      EApplication a t e1 es -> do
        EApplication a (applyTypeSub sub t) (applyTypeSub sub e1) (applyTypeSub sub es)
      EMatch a t e cs ->
        EMatch a (applyTypeSub sub t) (applyTypeSub sub e) (applyTypeSub sub cs)
      e@ELiteral{} ->
        e

newtype TypeSubstitution = TypeSubstitution {typeSubstitutionMap :: IndexMap OpaqueType}
  deriving (Show, Eq, Ord, Read)

instance Semigroup TypeSubstitution where
  s1 <> s2 = TypeSubstitution (s3 <> typeSubstitutionMap s1)
   where
    s3 = applyTypeSub s1 (typeSubstitutionMap s2)

instance Monoid TypeSubstitution where
  mempty = TypeSubstitution mempty

{-# INLINE mapsToType #-}
mapsToType :: Int -> OpaqueType -> TypeSubstitution
mapsToType index = TypeSubstitution . Map.singleton index

{-# INLINE removeTypeSubstitution #-}
removeTypeSubstitution :: TypeIndex () -> TypeSubstitution -> TypeSubstitution
removeTypeSubstitution TypeIndex{..} (TypeSubstitution sub) = TypeSubstitution (Map.delete typeIndexId sub)

{-# INLINE typeSubstitutionFromList #-}
typeSubstitutionFromList :: [(Int, OpaqueType)] -> TypeSubstitution
typeSubstitutionFromList = TypeSubstitution . Map.fromList

normalizeTypeIndexes :: (TypeSubstitutable s, TypeIndexed () s) => s -> s
normalizeTypeIndexes e = applyTypeSub (typeSubstitutionFromList sub) e
 where
  ixs = Set.toList (typeIndexesIn e)
  sub = [(ix, TVariable (TypeIndex () n)) | (n, TypeIndex _ ix) <- zip [0 ..] ixs]
