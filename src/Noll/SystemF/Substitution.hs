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
) where

import Data.Data (Data)
import Data.Generics.Uniplate.Data (transform, transformBi)
import Data.List.NonEmpty (NonEmpty)
import Noll.Label (Label (..))
import Noll.Language (
  Binding (..),
  Choice (..),
  Clause (..),
  CompiledClause (..),
  Constant (..),
  Definition (..),
  Expression (..),
  Function (..),
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
  Uses (..),
 )
import Noll.SystemF.Constraint (Constraint (..), MonomorphicSet (..))
import Noll.Utils (IndexMap, Map, Set, fromMaybe)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

class Substitutable s where
  apply :: Substitution -> s -> s

applyR :: Substitution -> Row TypeIndex Kind IndexedType -> Row TypeIndex Kind IndexedType
applyR sub =
    \case
      RVariable r ->
        case substitutionIndex r sub of
          Just (TRow row) ->
            row
          _ ->
            RVariable r
      r ->
        r 

applyT :: Substitution -> IndexedType -> IndexedType
applyT sub =
    \case
      TRow row ->
        TRow (applyR sub row)
      TVariable t ->
        fromMaybe (TVariable t) (substitutionIndex t sub)
      t ->
        t

instance Substitutable IndexedType where
  apply = transform . applyT

instance Substitutable (Row TypeIndex Kind IndexedType) where
  apply = transform . applyR

instance (Data s, Data k, Ord k) => Substitutable (Map k s) where
  apply = transformBi . applyT

instance (Data c) => Substitutable (Constraint c TypeIndex Kind IndexedType) where
  apply = transformBi . applyT

instance (Data s) => Substitutable [s] where
  apply = transformBi . applyT

instance (Data s) => Substitutable (NonEmpty s) where
  apply = transformBi . applyT

instance (Data s) => Substitutable (Maybe s) where
  apply = transformBi . applyT

instance (Data s) => Substitutable (Trait s) where
  apply = transformBi . applyT

instance (Ord s, Data s) => Substitutable (Set s) where
  apply = transformBi . applyT

instance Substitutable (MonomorphicSet (TypeIndex Kind)) where
  apply = transformBi . applyT

instance (Data s) => Substitutable (Intrinsic s) where
  apply = transformBi . applyT

instance (Data a) => Substitutable (Pattern a IndexedType) where
  apply = transformBi . applyT

instance (Data a) => Substitutable (Binding Expression a IndexedType) where
  apply = transformBi . applyT

instance (Data a) => Substitutable (Guard Expression a IndexedType) where
  apply = transformBi . applyT

instance (Data a) => Substitutable (Choice Expression a IndexedType) where
  apply = transformBi . applyT

instance (Data a) => Substitutable (Clause Expression a IndexedType) where
  apply = transformBi . applyT

newtype Substitution = Substitution {substitutionMap :: IndexMap IndexedType}
  deriving (Show, Eq, Ord, Read)

instance Substitutable (Scheme TypeIndex Kind IndexedType) where
  apply sub =
    \case
      Forall qs ps t ->
        Forall qs (apply sub1 ps) (apply sub1 t)
       where
        sub1 = foldr removeSubstitution sub qs

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
