{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.SystemF.Substitution (
  Substitutable (..),
  Substitution (..),
  mapsTo,
  fromList,
  normalizeTypeIndexes,
  applyT,
  merge,
) where

import Data.Map.Strict (Map, keysSet, union)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transform, transformBi)
import Data.Set (Set, intersection)
import Data.List.NonEmpty (NonEmpty)
import Lang.Utils (IndexMap, Map, Set, fromMaybe)
import Noll.Language (
  Binding (..),
  Choice (..),
  Clause (..),
  Expression (..),
  Guard (..),
  IndexedType,
  Intrinsic (..),
  Kind,
  Pattern (..),
  Row (..),
  Scheme (..),
  Trait (..),
  Type (..),
  TypeIndex (..),
  TypeIndexed (..),
 )
import Noll.Module (Constant (..), Definition (..), Function (..))
import Noll.SystemF.Constraint (Constraint (..), Monomorphic (..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

class Substitutable s where
  apply :: Substitution -> s -> s

applyT :: Substitution -> IndexedType -> IndexedType
applyT sub =
  \case
    TRow row ->
      TRow (transform (apply sub) row)
    TVariable t ->
      fromMaybe (TVariable t) (substitutionIndex t sub)
    t ->
      t

instance Substitutable IndexedType where
  apply = transform . applyT

instance Substitutable (Monomorphic (TypeIndex Kind)) where
  apply sub =
    \case
      Monomorphic m ->
        Monomorphic (typeIndexesIn (Set.map (apply sub . TVariable) m))

instance Substitutable (Scheme TypeIndex Kind IndexedType) where
  apply sub =
    \case
      Forall qs ps t ->
        Forall qs (apply sub1 ps) (apply sub1 t)
       where
        sub1 = foldr removeSubstitution sub qs

instance Substitutable (Row TypeIndex Kind IndexedType) where
  apply sub =
    \case
      RVariable r ->
        case substitutionIndex r sub of
          Just (TRow row) ->
            row
          _ ->
            RVariable r
      r ->
        r

instance Substitutable (Constraint c TypeIndex Kind IndexedType) where
  apply sub =
    \case
      Equality c ts ->
        Equality c (apply sub ts)
      Implicit c t1 t2 m ->
        Implicit c (apply sub t1) (apply sub t2) (apply sub m)
      Explicit c t1 s ->
        Explicit c (apply sub t1) (apply sub s)

instance (Substitutable s) => Substitutable (Map k s) where
  apply = fmap . apply

instance (Substitutable s) => Substitutable [s] where
  apply = fmap . apply

instance (Substitutable s) => Substitutable (NonEmpty s) where
  apply = fmap . apply

instance (Substitutable s) => Substitutable (Maybe s) where
  apply = fmap . apply

instance (Substitutable s) => Substitutable (Trait s) where
  apply = fmap . apply

instance (Ord s, Substitutable s) => Substitutable (Set s) where
  apply = Set.map . apply

instance (Data s) => Substitutable (Intrinsic s) where
  apply = transformBi . applyT

instance (Data a) => Substitutable (Pattern a IndexedType) where
  apply = transformBi . applyT

instance (Data a) => Substitutable (Expression a IndexedType) where
  apply = transformBi . applyT

instance (Data a) => Substitutable (Function Expression a IndexedType) where
  apply = transformBi . applyT

instance (Data a) => Substitutable (Constant Expression a IndexedType) where
  apply = transformBi . applyT

instance (Data a, Data k, Ord k) => Substitutable (Definition a k IndexedType) where
  apply = transformBi . applyT

instance (Data a) => Substitutable (Binding Expression a IndexedType) where
  apply = transformBi . applyT

instance (Data a) => Substitutable (Guard Expression a IndexedType) where
  apply = transformBi . applyT

instance (Data a) => Substitutable (Choice Expression a IndexedType) where
  apply = transformBi . applyT

instance (Data a) => Substitutable (Clause a IndexedType) where
  apply = transformBi . applyT

newtype Substitution = Substitution {substitutionMap :: IndexMap IndexedType}
  deriving (Show, Eq, Ord, Read)

instance Semigroup Substitution where
  s1 <> s2 = Substitution (s3 <> substitutionMap s1)
   where
    s3 = apply s1 (substitutionMap s2)

instance Monoid Substitution where
  mempty = Substitution mempty

{-# INLINE substitutionIndex #-}
substitutionIndex :: TypeIndex Kind -> Substitution -> Maybe IndexedType
substitutionIndex TypeIndex{..} Substitution{..} = Map.lookup typeIndexId substitutionMap

{-# INLINE removeSubstitution #-}
removeSubstitution :: TypeIndex Kind -> Substitution -> Substitution
removeSubstitution TypeIndex{..} Substitution{..} = Substitution (Map.delete typeIndexId substitutionMap)

{-# INLINE mapsTo #-}
mapsTo :: Int -> IndexedType -> Substitution
mapsTo index = Substitution . Map.singleton index

{-# INLINE fromList #-}
fromList :: [(Int, IndexedType)] -> Substitution
fromList = Substitution . Map.fromList

normalizeTypeIndexes :: (Substitutable s, TypeIndexed Kind s) => s -> s
normalizeTypeIndexes a = apply (fromList sub) a
 where
  sub = do
    (n, TypeIndex k t) <- zip [0 ..] (Set.toList (typeIndexesIn a))
    pure (t, TVariable (TypeIndex k n))

merge :: Substitution -> Substitution -> Maybe Substitution
merge (Substitution m1) (Substitution m2)
  | restricted m1 == restricted m2 = Just (Substitution (m1 `union` m2))
  | otherwise = Nothing
 where
  restricted = (`Map.restrictKeys` keys)
  keys = keysSet m1 `intersection` keysSet m2
