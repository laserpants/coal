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

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Noll.Label (Label (..))
import Noll.Language (
  Binding (..),
  Expression (..),
  HasTypeIndexes (..),
  Kind (..),
  KindIndex (..),
  Pattern (..),
  Row (..),
  Scheme (..),
  Trait (..),
  Type (..),
  TypeIndex (..),
 )
import Noll.TypeSystem.TypeConstraint (MonomorphicSet (..), TypeConstraint (..))
import Noll.Utils (IndexMap, Map, NonEmpty, Set)

class TypeSubstitutable s where
  apply :: TypeSubstitution -> s -> s

instance (TypeSubstitutable s) => TypeSubstitutable (Map k s) where
  apply = fmap . apply

instance (TypeSubstitutable s) => TypeSubstitutable [s] where
  apply = fmap . apply

instance (TypeSubstitutable s) => TypeSubstitutable (NonEmpty s) where
  apply = fmap . apply

instance (TypeSubstitutable s) => TypeSubstitutable (Maybe s) where
  apply = fmap . apply

instance (TypeSubstitutable s) => TypeSubstitutable (Trait s) where
  apply = fmap . apply

instance (Ord s, TypeSubstitutable s) => TypeSubstitutable (Set s) where
  apply = Set.map . apply

instance TypeSubstitutable (Row TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))) where
  apply sub =
    error "TODO"

instance TypeSubstitutable (MonomorphicSet (TypeIndex (Kind KindIndex))) where
  apply sub =
    \case
      MonomorphicSet m ->
        MonomorphicSet (typeIndexesIn (Set.map (apply sub . TVariable) m))

instance TypeSubstitutable (Scheme TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))) where
  apply sub =
    \case
      Forall qs ps t ->
        let
          sub1 = foldr removeTypeSubstitution sub qs
         in
          Forall qs (apply sub1 ps) (apply sub1 t)

instance TypeSubstitutable (TypeConstraint c TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))) where
  apply sub =
    \case
      Equality meta t1 t2 ->
        Equality meta (apply sub t1) (apply sub t2)
      Implicit meta t1 t2 m ->
        Implicit meta (apply sub t1) (apply sub t2) (apply sub m)
      Explicit meta t1 s ->
        Explicit meta (apply sub t1) (apply sub s)

instance TypeSubstitutable (Type TypeIndex (Kind KindIndex)) where
  apply sub =
    \case
      TAlias name ts t -> do
        TAlias name (apply sub ts) (apply sub t)
      TApplication k t1 ts ->
        TApplication k (apply sub t1) (apply sub ts)
      TArrow t1 t2 ->
        TArrow (apply sub t1) (apply sub t2)
      TIntrinsic t ->
        TIntrinsic (apply sub <$> t)
      TRow row ->
        TRow (apply sub row)
      TVariable t ->
        fromMaybe (TVariable t) (substitutionIndex t sub)
      t@TConstructor{} ->
        t

{-# INLINE substitutionIndex #-}
substitutionIndex :: TypeIndex (Kind KindIndex) -> TypeSubstitution -> Maybe (Type TypeIndex (Kind KindIndex))
substitutionIndex TypeIndex{..} sub = Map.lookup indexId (typeSubstitutionMap sub)

instance TypeSubstitutable (Pattern a (Type TypeIndex (Kind KindIndex))) where
  apply sub =
    \case
      PVariable a (Label t name) ->
        PVariable a (Label (apply sub t) name)

instance TypeSubstitutable (Binding Expression a (Type TypeIndex (Kind KindIndex))) where
  apply sub =
    \case
      BPattern a p e ->
        BPattern a (apply sub p) (apply sub e)

instance TypeSubstitutable (Expression a (Type TypeIndex (Kind KindIndex))) where
  apply sub =
    \case
      EConstructor a (Label t name) -> do
        EConstructor a (Label (apply sub t) name)
      EVariable a (Label t name) -> do
        EVariable a (Label (apply sub t) name)
      ELambda a ps e -> do
        ELambda a (apply sub ps) (apply sub e)
      ELet a gs e1 -> do
        ELet a (apply sub gs) (apply sub e1)
      EIf a t e1 e2 e3 -> do
        EIf a (apply sub t) (apply sub e1) (apply sub e2) (apply sub e3)
      EApplication a t e1 es -> do
        EApplication a (apply sub t) (apply sub e1) (apply sub es)
      e@ELiteral{} ->
        e

newtype TypeSubstitution = TypeSubstitution {typeSubstitutionMap :: IndexMap (Type TypeIndex (Kind KindIndex))}
  deriving (Show, Eq, Ord, Read)

instance Semigroup TypeSubstitution where
  s1 <> s2 = TypeSubstitution (s3 <> typeSubstitutionMap s1)
   where
    s3 = apply s1 (typeSubstitutionMap s2)

instance Monoid TypeSubstitution where
  mempty = TypeSubstitution mempty

{-# INLINE mapsToType #-}
mapsToType :: Int -> Type TypeIndex (Kind KindIndex) -> TypeSubstitution
mapsToType index = TypeSubstitution . Map.singleton index

{-# INLINE removeTypeSubstitution #-}
removeTypeSubstitution :: TypeIndex (Kind KindIndex) -> TypeSubstitution -> TypeSubstitution
removeTypeSubstitution TypeIndex{..} (TypeSubstitution sub) = TypeSubstitution (Map.delete indexId sub)

{-# INLINE typeSubstitutionFromList #-}
typeSubstitutionFromList :: [(Int, Type TypeIndex (Kind KindIndex))] -> TypeSubstitution
typeSubstitutionFromList = TypeSubstitution . Map.fromList

normalizeTypeIndexes :: (TypeSubstitutable s, HasTypeIndexes (Kind KindIndex) s) => s -> s
normalizeTypeIndexes e = apply (typeSubstitutionFromList sub) e
 where
  ixs = Set.toList (typeIndexesIn e)
  sub = [(ix, TVariable (TypeIndex k n)) | (n, TypeIndex k ix) <- zip [0 ..] ixs]
