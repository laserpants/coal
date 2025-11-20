{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.TypeSystem.Substitution (
  Substitutable (..),
  Substitution (..),
  mapsTo,
  fromList,
  normalizeTypeIndexes,
  applyT,
  merge,
) where

import Coal.Common.Environment (Environment (..))
import Coal.Language
import Coal.Language.Module (ConstantDef (..), Definition (..), FunctionDef (..))
import Coal.TypeSystem.Constraint (Constraint (..), Monomorphic (..))
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transform, transformBi)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map, keysSet, union)
import qualified Data.Map.Strict as Map
import Data.Set (Set, intersection)
import qualified Data.Set as Set
import Extras (fromMaybe)

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

instance Substitutable IndexedScheme where
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
          Just (TVariable t) ->
            RVariable t
          _ ->
            RVariable r
      RExtend name t r ->
        RExtend name (apply sub t) (apply sub r)
      RNil ->
        RNil

instance Substitutable (Constraint c TypeIndex Kind IndexedType) where
  apply sub =
    \case
      Equality c ts ->
        Equality c (apply sub ts)
      Implicit c t1 t2 m ->
        Implicit c (apply sub t1) (apply sub t2) (apply sub m)
      Explicit c t1 s ->
        Explicit c (apply sub t1) (apply sub s)
      Lacks c t name ->
        Lacks c (apply sub t) name

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

instance Substitutable Intrinsic where
  apply _ s = s

instance (Data a) => Substitutable (Pattern a IndexedType) where
  apply = transformBi . applyT

instance (Data a) => Substitutable (Expression a IndexedType) where
  apply = transformBi . applyT

instance (Data a) => Substitutable (FunctionDef a IndexedType) where
  apply = transformBi . applyT

instance (Data a) => Substitutable (ConstantDef a IndexedType) where
  apply = transformBi . applyT

instance (Data a, Data k) => Substitutable (Definition a k IndexedType) where
  apply = transformBi . applyT

instance (Data a) => Substitutable (Binding Expression a IndexedType) where
  apply = transformBi . applyT

instance (Data a) => Substitutable (Guard Expression a IndexedType) where
  apply = transformBi . applyT

instance (Data a) => Substitutable (Choice Expression a IndexedType) where
  apply = transformBi . applyT

instance (Data a) => Substitutable (Clause a IndexedType) where
  apply = transformBi . applyT

newtype Substitution = Substitution {substitutionMap :: Map Int IndexedType}
  deriving (Show, Eq, Ord, Read)

instance Semigroup Substitution where
  s1 <> s2 = Substitution (s3 <> substitutionMap s1)
   where
    s3 = apply s1 (substitutionMap s2)

instance Monoid Substitution where
  mempty = Substitution mempty

instance (Substitutable s) => Substitutable (Environment s) where
  apply sub =
    \case
      Environment e ->
        Environment (apply sub e)

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
