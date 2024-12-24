{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Noll.Language.HasTypeIndexes (HasTypeIndexes (..), typeIdsIn, notBoundIn, freshIdIn) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set, singleton)
import qualified Data.Set as Set
import Noll.Label (Label (..))
import Noll.Language.Pattern (Pattern)
import qualified Noll.Language.Pattern as Pattern
import Noll.Language.Trait (Trait (..))
import Noll.Language.Type (Type)
import qualified Noll.Language.Type as Type
import Noll.Language.Type.Index (TypeIndex (..))
import Noll.Language.Type.Kind (Kind)
import Noll.Language.Type.Row (Row)
import qualified Noll.Language.Type.Row as Row
import Noll.Language.Type.Scheme (Scheme (..))

class HasTypeIndexes k t | t -> k where
  typeIndexesIn :: t -> Set (TypeIndex k)

instance HasTypeIndexes k (TypeIndex k) where
  typeIndexesIn = singleton

instance (Ord k, HasTypeIndexes k t) => HasTypeIndexes k (Map a t) where
  typeIndexesIn = Set.unions . fmap typeIndexesIn

instance (Ord k, HasTypeIndexes k t) => HasTypeIndexes k (Maybe t) where
  typeIndexesIn = Set.unions . fmap typeIndexesIn

instance (Ord k, HasTypeIndexes k t) => HasTypeIndexes k [t] where
  typeIndexesIn = Set.unions . fmap typeIndexesIn

instance (Ord k, HasTypeIndexes k t) => HasTypeIndexes k (NonEmpty t) where
  typeIndexesIn = Set.unions . fmap typeIndexesIn

instance (Ord k, HasTypeIndexes k t) => HasTypeIndexes k (Trait t) where
  typeIndexesIn = Set.unions . fmap typeIndexesIn

instance (Ord k, HasTypeIndexes k t) => HasTypeIndexes k (Set t) where
  typeIndexesIn = Set.unions . Set.map typeIndexesIn

instance (HasTypeIndexes k t) => HasTypeIndexes k (Label t) where
  typeIndexesIn =
    \case
      Label t _ ->
        typeIndexesIn t

instance (Ord k, HasTypeIndexes k t) => HasTypeIndexes k (Row TypeIndex k t) where
  typeIndexesIn =
    \case
      Row.Extend _ t row ->
        typeIndexesIn t <> typeIndexesIn row
      Row.Variable t ->
        typeIndexesIn t
      Row.Nil ->
        mempty

instance (Ord k) => HasTypeIndexes k (Type TypeIndex k) where
  typeIndexesIn =
    \case
      Type.Application _ t ts ->
        typeIndexesIn t <> typeIndexesIn ts
      Type.Arrow t1 t2 ->
        typeIndexesIn t1 <> typeIndexesIn t2
      Type.Constructor{} ->
        mempty
      Type.Intrinsic{} ->
        mempty
      Type.Row row ->
        typeIndexesIn row
      Type.Variable t ->
        typeIndexesIn t
      Type.Alias _ _ t ->
        typeIndexesIn t

instance (HasTypeIndexes k t) => HasTypeIndexes k (Pattern t) where
  typeIndexesIn =
    \case
      Pattern.Variable (Label t _) ->
        typeIndexesIn t

instance (Ord k, HasTypeIndexes k t) => HasTypeIndexes k (Scheme TypeIndex k t) where
  typeIndexesIn =
    \case
      Forall qs ps t ->
        notBoundIn qs (typeIndexesIn t <> typeIndexesIn ps)

notBoundIn :: Set (TypeIndex k) -> Set (TypeIndex k) -> Set (TypeIndex k)
notBoundIn s = Set.filter notBound
 where
  notBound index = indexId index `notElem` Set.map indexId s

typeIdsIn :: (HasTypeIndexes k t) => t -> Set Int
typeIdsIn = Set.map indexId . typeIndexesIn

freshIdIn :: (HasTypeIndexes k t) => t -> Int
freshIdIn t =
  if null typeIdSet
    then 0
    else succ (maximum typeIdSet)
 where
  typeIdSet = typeIdsIn t
