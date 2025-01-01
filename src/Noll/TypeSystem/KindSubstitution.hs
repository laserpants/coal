{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.KindSubstitution (
  KindSubstitution (..),
  KindSubstitutable (..),
  mapsToKind,
) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Noll.Language (Expression (..), IndexedType, Kind (..), KindIndex (..), Row, Trait (..), Type (..), TypeIndex (..))
import Noll.TypeSystem.KindConstraint (KindConstraint (..))
import Noll.Utils (IndexMap, Map, Set)

class KindSubstitutable s where
  apply :: KindSubstitution -> s -> s

instance (KindSubstitutable s) => KindSubstitutable (Map k s) where
  apply = fmap . apply

instance (KindSubstitutable s) => KindSubstitutable [s] where
  apply = fmap . apply

instance (KindSubstitutable s) => KindSubstitutable (NonEmpty s) where
  apply = fmap . apply

instance (KindSubstitutable s) => KindSubstitutable (Maybe s) where
  apply = fmap . apply

instance (KindSubstitutable s) => KindSubstitutable (Trait s) where
  apply = fmap . apply

instance (Ord s, KindSubstitutable s) => KindSubstitutable (Set s) where
  apply = Set.map . apply

instance KindSubstitutable (Kind KindIndex) where
  apply sub =
    \case
      KArrow k1 k2 ->
        KArrow (apply sub k1) (apply sub k2)
      KVariable k ->
        fromMaybe (KVariable k) (substitutionIndex k sub)
      k ->
        k

{-# INLINE substitutionIndex #-}
substitutionIndex :: KindIndex -> KindSubstitution -> Maybe (Kind KindIndex)
substitutionIndex (KindIndex index) sub = Map.lookup index (kindSubstitutionMap sub)

instance KindSubstitutable (TypeIndex (Kind KindIndex)) where
  apply sub =
    \case
      TypeIndex k index ->
        TypeIndex (apply sub k) index

instance KindSubstitutable (Row TypeIndex (Kind KindIndex) IndexedType) where
  apply sub =
    error "TODO"

instance KindSubstitutable IndexedType where
  apply sub =
    \case
      TAlias name ts t -> do
        TAlias name (apply sub ts) (apply sub t)
      TApplication k t1 ts ->
        TApplication (apply sub k) (apply sub t1) (apply sub ts)
      TArrow t1 t2 ->
        TArrow (apply sub t1) (apply sub t2)
      TIntrinsic t ->
        TIntrinsic (apply sub <$> t)
      TRow row ->
        TRow (apply sub row)
      TVariable t ->
        TVariable (apply sub t)
      TConstructor k name ->
        TConstructor (apply sub k) name

instance (KindSubstitutable k) => KindSubstitutable (KindConstraint c k) where
  apply sub =
    \case
      KindEquality meta k1 k2 ->
        KindEquality meta (apply sub k1) (apply sub k2)

instance KindSubstitutable (Expression a IndexedType) where
  apply = fmap . apply

newtype KindSubstitution = KindSubstitution {kindSubstitutionMap :: IndexMap (Kind KindIndex)}
  deriving (Show, Eq, Ord, Read)

instance Semigroup KindSubstitution where
  s1 <> s2 = KindSubstitution (s3 <> kindSubstitutionMap s1)
   where
    s3 = apply s1 (kindSubstitutionMap s2)

instance Monoid KindSubstitution where
  mempty = KindSubstitution mempty

{-# INLINE mapsToKind #-}
mapsToKind :: Int -> Kind KindIndex -> KindSubstitution
mapsToKind index = KindSubstitution . Map.singleton index
