{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Substitution (TypeSubstitution (..), TypeSubstitutable (..), apply, mapsTo) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Noll.Language.Trait (Trait (..))
import Noll.Language.Type (Type)
import qualified Noll.Language.Type as Type
import Noll.Language.Type.Index (TypeIndex (..))
import Noll.Language.Type.Kind (Kind)
import Noll.Language.Type.Row (Row (..))
import Noll.Utils (IndexMap)

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

instance TypeSubstitutable (Row TypeIndex (Kind Int) (Type TypeIndex (Kind Int))) where
  apply sub =
    undefined

instance TypeSubstitutable (Type TypeIndex (Kind Int)) where
  apply sub =
    \case
      Type.Alias name ts t -> do
        Type.Alias name (apply sub ts) (apply sub t)
      Type.Application k t1 ts ->
        Type.Application k (apply sub t1) (apply sub ts)
      Type.Arrow t1 t2 ->
        Type.Arrow (apply sub t1) (apply sub t2)
      Type.Intrinsic t ->
        Type.Intrinsic (apply sub <$> t)
      Type.Row row ->
        Type.Row (apply sub row)
      Type.Variable t ->
        fromMaybe (Type.Variable t) (substitutionIndex t sub)
      t@Type.Constructor{} ->
        t

newtype TypeSubstitution = TypeSubstitution {typeSubstitutionMap :: IndexMap (Type TypeIndex (Kind Int))}
  deriving (Show, Eq, Ord, Read)

instance Semigroup TypeSubstitution where
  s1 <> s2 = TypeSubstitution (s3 <> typeSubstitutionMap s1)
   where
    s3 = apply s1 (typeSubstitutionMap s2)

instance Monoid TypeSubstitution where
  mempty = TypeSubstitution mempty

{-# INLINE substitutionIndex #-}
substitutionIndex :: TypeIndex k -> TypeSubstitution -> Maybe (Type TypeIndex (Kind Int))
substitutionIndex TypeIndex{..} sub = Map.lookup indexId (typeSubstitutionMap sub)

{-# INLINE mapsTo #-}
mapsTo :: Int -> Type TypeIndex (Kind Int) -> TypeSubstitution
mapsTo index = TypeSubstitution . Map.singleton index
