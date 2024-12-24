{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.KindSubstitution (
  KindSubstitution (..),
  KindSubstitutable (..),
  kindMapsTo,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Noll.Language.Expression (Expression (..))
import Noll.Language.Kind.Index (KindIndex (..))
import Noll.Language.Trait (Trait (..))
import Noll.Language.Type (Type)
import qualified Noll.Language.Type as Type
import Noll.Language.Type.Index (TypeIndex (..))
import Noll.Language.Type.Kind (Kind)
import qualified Noll.Language.Type.Kind as Kind
import Noll.Language.Type.Row (Row (..))
import Noll.Utils (IndexMap)

class KindSubstitutable s where
  kindApply :: KindSubstitution -> s -> s

instance (KindSubstitutable s) => KindSubstitutable (Map k s) where
  kindApply = fmap . kindApply

instance (KindSubstitutable s) => KindSubstitutable [s] where
  kindApply = fmap . kindApply

instance (KindSubstitutable s) => KindSubstitutable (NonEmpty s) where
  kindApply = fmap . kindApply

instance (KindSubstitutable s) => KindSubstitutable (Maybe s) where
  kindApply = fmap . kindApply

instance (KindSubstitutable s) => KindSubstitutable (Trait s) where
  kindApply = fmap . kindApply

instance (Ord s, KindSubstitutable s) => KindSubstitutable (Set s) where
  kindApply = Set.map . kindApply

instance KindSubstitutable (Kind KindIndex) where
  kindApply sub =
    \case
      Kind.Arrow k1 k2 ->
        Kind.Arrow (kindApply sub k1) (kindApply sub k2)
      Kind.Variable k ->
        fromMaybe (Kind.Variable k) (substitutionIndex k sub)
      k ->
        k

instance KindSubstitutable (TypeIndex (Kind KindIndex)) where
  kindApply sub =
    \case
      TypeIndex k index ->
        TypeIndex (kindApply sub k) index

instance KindSubstitutable (Row TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))) where
  kindApply sub =
    undefined

instance KindSubstitutable (Type TypeIndex (Kind KindIndex)) where
  kindApply sub =
    \case
      Type.Alias name ts t -> do
        Type.Alias name (kindApply sub ts) (kindApply sub t)
      Type.Application k t1 ts ->
        Type.Application (kindApply sub k) (kindApply sub t1) (kindApply sub ts)
      Type.Arrow t1 t2 ->
        Type.Arrow (kindApply sub t1) (kindApply sub t2)
      Type.Intrinsic t ->
        Type.Intrinsic (kindApply sub <$> t)
      Type.Row row ->
        Type.Row (kindApply sub row)
      Type.Variable t ->
        Type.Variable (kindApply sub t)
      Type.Constructor k name ->
        Type.Constructor (kindApply sub k) name

instance KindSubstitutable (Expression (Type TypeIndex (Kind KindIndex))) where
  kindApply = fmap . kindApply

newtype KindSubstitution = KindSubstitution {kindSubstitutionMap :: IndexMap (Kind KindIndex)}
  deriving (Show, Eq, Ord, Read)

instance Semigroup KindSubstitution where
  s1 <> s2 = KindSubstitution (s3 <> kindSubstitutionMap s1)
   where
    s3 = kindApply s1 (kindSubstitutionMap s2)

instance Monoid KindSubstitution where
  mempty = KindSubstitution mempty

{-# INLINE substitutionIndex #-}
substitutionIndex :: KindIndex -> KindSubstitution -> Maybe (Kind KindIndex)
substitutionIndex (KindIndex index) sub = Map.lookup index (kindSubstitutionMap sub)

{-# INLINE kindMapsTo #-}
kindMapsTo :: Int -> Kind KindIndex -> KindSubstitution
kindMapsTo index = KindSubstitution . Map.singleton index
