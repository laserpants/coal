{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.KindSubstitution (
  KindSubstitution (..),
  KindSubstitutable (..),
  mapsToKind,
) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Noll.Language (Expression (..), IndexedType, Kind (..), KindIndex (..), Row, Trait (..), Type (..), TypeIndex (..))
import Noll.TypeSystem.KindConstraint (KindConstraint (..))
import Noll.Utils (IndexMap, Map, NonEmpty, Set)

class KindSubstitutable s where
  applyKindSub :: KindSubstitution -> s -> s

instance (KindSubstitutable s) => KindSubstitutable (Map k s) where
  applyKindSub = fmap . applyKindSub

instance (KindSubstitutable s) => KindSubstitutable [s] where
  applyKindSub = fmap . applyKindSub

instance (KindSubstitutable s) => KindSubstitutable (NonEmpty s) where
  applyKindSub = fmap . applyKindSub

instance (KindSubstitutable s) => KindSubstitutable (Maybe s) where
  applyKindSub = fmap . applyKindSub

instance (KindSubstitutable s) => KindSubstitutable (Trait s) where
  applyKindSub = fmap . applyKindSub

instance (Ord s, KindSubstitutable s) => KindSubstitutable (Set s) where
  applyKindSub = Set.map . applyKindSub

instance KindSubstitutable (Kind KindIndex) where
  applyKindSub sub =
    \case
      KArrow k1 k2 ->
        KArrow (applyKindSub sub k1) (applyKindSub sub k2)
      KVariable k ->
        fromMaybe (KVariable k) (substitutionIndex k sub)
      k ->
        k

{-# INLINE substitutionIndex #-}
substitutionIndex :: KindIndex -> KindSubstitution -> Maybe (Kind KindIndex)
substitutionIndex (KindIndex index) sub = Map.lookup index (kindSubstitutionMap sub)

instance KindSubstitutable (TypeIndex (Kind KindIndex)) where
  applyKindSub sub =
    \case
      TypeIndex k index ->
        TypeIndex (applyKindSub sub k) index

instance KindSubstitutable (Row TypeIndex (Kind KindIndex) IndexedType) where
  applyKindSub sub =
    error "TODO"

instance KindSubstitutable IndexedType where
  applyKindSub sub =
    \case
      TAlias name ts t -> do
        TAlias name (applyKindSub sub ts) (applyKindSub sub t)
      TApplication k t1 ts ->
        TApplication (applyKindSub sub k) (applyKindSub sub t1) (applyKindSub sub ts)
      TArrow t1 t2 ->
        TArrow (applyKindSub sub t1) (applyKindSub sub t2)
      TIntrinsic t ->
        TIntrinsic (applyKindSub sub <$> t)
      TRow row ->
        TRow (applyKindSub sub row)
      TVariable t ->
        TVariable (applyKindSub sub t)
      TConstructor k name ->
        TConstructor (applyKindSub sub k) name

instance (KindSubstitutable k) => KindSubstitutable (KindConstraint c k) where
  applyKindSub sub =
    \case
      KindEquality meta k1 k2 ->
        KindEquality meta (applyKindSub sub k1) (applyKindSub sub k2)

instance KindSubstitutable (Expression a IndexedType) where
  applyKindSub = fmap . applyKindSub

newtype KindSubstitution = KindSubstitution {kindSubstitutionMap :: IndexMap (Kind KindIndex)}
  deriving (Show, Eq, Ord, Read)

instance Semigroup KindSubstitution where
  s1 <> s2 = KindSubstitution (s3 <> kindSubstitutionMap s1)
   where
    s3 = applyKindSub s1 (kindSubstitutionMap s2)

instance Monoid KindSubstitution where
  mempty = KindSubstitution mempty

{-# INLINE mapsToKind #-}
mapsToKind :: Int -> Kind KindIndex -> KindSubstitution
mapsToKind index = KindSubstitution . Map.singleton index
