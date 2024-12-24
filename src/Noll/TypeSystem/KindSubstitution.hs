{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.KindSubstitution (
  KindSubstitution (..),
  KindSubstitutable (..),
  mapsToKindSub,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Noll.Language.Expression (Expression (..))
import Noll.Language.Trait (Trait (..))
import Noll.Language.Type (Type)
import qualified Noll.Language.Type as Type
import Noll.Language.Type.Index (TypeIndex (..))
import Noll.Language.Type.Kind (Kind)
import qualified Noll.Language.Type.Kind as Kind
import Noll.Language.Type.Kind.Index (KindIndex (..))
import Noll.Language.Type.Row (Row (..))
import Noll.Utils (IndexMap)

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
      Kind.Arrow k1 k2 ->
        Kind.Arrow (applyKindSub sub k1) (applyKindSub sub k2)
      Kind.Variable k ->
        fromMaybe (Kind.Variable k) (substitutionIndex k sub)
      k ->
        k

instance KindSubstitutable (TypeIndex (Kind KindIndex)) where
  applyKindSub sub =
    \case
      TypeIndex k index ->
        TypeIndex (applyKindSub sub k) index

instance KindSubstitutable (Row TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))) where
  applyKindSub sub =
    undefined

instance KindSubstitutable (Type TypeIndex (Kind KindIndex)) where
  applyKindSub sub =
    \case
      Type.Alias name ts t -> do
        Type.Alias name (applyKindSub sub ts) (applyKindSub sub t)
      Type.Application k t1 ts ->
        Type.Application (applyKindSub sub k) (applyKindSub sub t1) (applyKindSub sub ts)
      Type.Arrow t1 t2 ->
        Type.Arrow (applyKindSub sub t1) (applyKindSub sub t2)
      Type.Intrinsic t ->
        Type.Intrinsic (applyKindSub sub <$> t)
      Type.Row row ->
        Type.Row (applyKindSub sub row)
      Type.Variable t ->
        Type.Variable (applyKindSub sub t)
      Type.Constructor k name ->
        Type.Constructor (applyKindSub sub k) name

instance KindSubstitutable (Expression (Type TypeIndex (Kind KindIndex))) where
  applyKindSub = fmap . applyKindSub

newtype KindSubstitution = KindSubstitution {kindSubstitutionMap :: IndexMap (Kind KindIndex)}
  deriving (Show, Eq, Ord, Read)

instance Semigroup KindSubstitution where
  s1 <> s2 = KindSubstitution (s3 <> kindSubstitutionMap s1)
   where
    s3 = applyKindSub s1 (kindSubstitutionMap s2)

instance Monoid KindSubstitution where
  mempty = KindSubstitution mempty

{-# INLINE substitutionIndex #-}
substitutionIndex :: KindIndex -> KindSubstitution -> Maybe (Kind KindIndex)
substitutionIndex (KindIndex index) sub = Map.lookup index (kindSubstitutionMap sub)

{-# INLINE mapsToKindSub #-}
mapsToKindSub :: Int -> Kind KindIndex -> KindSubstitution
mapsToKindSub index = KindSubstitution . Map.singleton index
