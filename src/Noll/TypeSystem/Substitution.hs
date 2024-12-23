{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Substitution where

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
import Noll.Language.Type.Row (Row (..))
import Noll.Utils (IndexMap)

class TypeSubstitutable s k where
  apply :: TypeSubstitution k -> s -> s

instance (TypeSubstitutable s k) => TypeSubstitutable (Map a s) k where
  apply = fmap . apply

instance (TypeSubstitutable s k) => TypeSubstitutable [s] k where
  apply = fmap . apply

instance (TypeSubstitutable s k) => TypeSubstitutable (NonEmpty s) k where
  apply = fmap . apply

instance (TypeSubstitutable s k) => TypeSubstitutable (Maybe s) k where
  apply = fmap . apply

instance (TypeSubstitutable s k) => TypeSubstitutable (Trait s) k where
  apply = fmap . apply

instance (Ord s, TypeSubstitutable s k) => TypeSubstitutable (Set s) k where
  apply = Set.map . apply

instance TypeSubstitutable (Row TypeIndex k (Type TypeIndex k)) k where
  apply sub =
    undefined

instance TypeSubstitutable (Type TypeIndex k) k where
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

newtype TypeSubstitution k = TypeSubstitution {typeSubstitutionMap :: IndexMap (Type TypeIndex k)}
  deriving (Show, Eq, Ord, Read)

instance Semigroup (TypeSubstitution k) where
  s1 <> s2 = TypeSubstitution (s3 <> typeSubstitutionMap s1)
   where
    s3 = apply s1 (typeSubstitutionMap s2)

instance Monoid (TypeSubstitution k) where
  mempty = TypeSubstitution mempty

{-# INLINE substitutionIndex #-}
substitutionIndex :: TypeIndex k -> TypeSubstitution k -> Maybe (Type TypeIndex k)
substitutionIndex TypeIndex{..} sub = Map.lookup indexId (typeSubstitutionMap sub)

{-# INLINE mapsTo #-}
mapsTo :: Int -> Type TypeIndex k -> TypeSubstitution k
mapsTo index = TypeSubstitution . Map.singleton index
