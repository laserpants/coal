{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

module Noll.TypeSystem.Substitution where

import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Noll.Language.Trait (Trait (..))
import Noll.Language.Type (Type)
import Noll.Language.Type.Row (Row (..))
import qualified Noll.Language.Type as Type
import Noll.Language.Type.Index (TypeIndex (..))
import Noll.Utils (IndexMap)

--class Substitutable s t where
--  apply :: Substitution t -> s -> s
--
--instance (Substitutable s t) => Substitutable (Map a s) t where
--  apply = fmap . apply
--
--instance (Substitutable s t) => Substitutable [s] t where
--  apply = fmap . apply
--
--instance (Substitutable s t) => Substitutable (NonEmpty s) t where
--  apply = fmap . apply
--
--instance (Substitutable s t) => Substitutable (Maybe s) t where
--  apply = fmap . apply
--
--instance (Substitutable s t) => Substitutable (Trait s) t where
--  apply = fmap . apply
--
--instance (Substitutable s t) => Substitutable (TypeIndex s) t where
--  apply = fmap . apply
--
--instance (Ord s, Substitutable s t) => Substitutable (Set s) t where
--  apply = Set.map . apply
--
--instance Substitutable () t where
--  apply _ = id
--
--instance Substitutable (Row TypeIndex s (Type TypeIndex s)) t where
--  apply =
--    undefined
--
--instance (Substitutable s (Type TypeIndex s)) => Substitutable (Type TypeIndex s) (Type TypeIndex s) where
--  apply sub =
--    \case
--      Type.Alias name ts t ->
--        Type.Alias name (apply sub ts) (apply sub t)
--      Type.Application k t1 ts ->
--        Type.Application k (apply sub t1) (apply sub ts)
--      Type.Arrow t1 t2 ->
--        Type.Arrow (apply sub t1) (apply sub t2)
--      Type.Intrinsic t ->
--        Type.Intrinsic (apply sub <$> t)
--      Type.Row row ->
--        Type.Row (apply sub row)
----      Type.Variable t ->
----        fromMaybe (Type.Variable t) (substitutionIndex t sub)
----      t@Type.Constructor{} ->
----        t
--

class TypeSubstitutable s where
  apply :: TypeSubstitution -> s -> s

instance (TypeSubstitutable s) => TypeSubstitutable (Map a s) where
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

instance TypeSubstitutable (TypeIndex ()) where
  apply = undefined

--instance TypeSubstitutable (TypeIndex ()) where
--  apply sub =
--    \case
--      TypeIndex _ t ->
--        -- undefined -- fromMaybe (Type.Variable undefined) (substitutionIndex2 t sub)
--        fromMaybe undefined (substitutionIndex2 t sub)

instance TypeSubstitutable (Row TypeIndex () (Type TypeIndex ())) where
  apply sub =
    undefined

instance TypeSubstitutable (Type TypeIndex ()) where
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
--      Type.Variable t ->
--        Type.Variable (apply sub t)
--      Type.Variable t ->
--        fromMaybe (Type.Variable t) (substitutionIndex t sub)
      t@Type.Constructor{} ->
        t

newtype TypeSubstitution = TypeSubstitution {typeSubstitutionMap :: IndexMap (Type TypeIndex ())}
  deriving (Show, Eq, Ord, Read)

instance Semigroup TypeSubstitution where
  s1 <> s2 = TypeSubstitution (s3 <> typeSubstitutionMap s1)
   where
    s3 = apply s1 (typeSubstitutionMap s2)

instance Monoid TypeSubstitution where
  mempty = TypeSubstitution mempty

substitutionIndex :: TypeIndex () -> TypeSubstitution -> Maybe (Type TypeIndex ())
substitutionIndex TypeIndex{..} sub = Map.lookup indexId (typeSubstitutionMap sub)

--substitutionIndex :: TypeIndex () -> TypeSubstitution -> Maybe (Type TypeIndex ())
substitutionIndex2 :: Int -> TypeSubstitution -> Maybe (Type TypeIndex ())
substitutionIndex2 index sub = Map.lookup index (typeSubstitutionMap sub)

{-# INLINE mapsTo #-}
mapsTo :: Int -> Type TypeIndex () -> TypeSubstitution
mapsTo index = TypeSubstitution . Map.singleton index
