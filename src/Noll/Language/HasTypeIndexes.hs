{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.HasTypeIndexes (HasTypeIndexes (..)) where

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
import Noll.Language.Type.Row (Row)
import qualified Noll.Language.Type.Row as Row
import Noll.Language.Type.Scheme (Scheme (..))
import Noll.TypeSystem.Constraint (MonomorphicSet (..))

class HasTypeIndexes o k t where
  typeIndexesIn :: t -> Set (o k)

instance HasTypeIndexes o k (o k) where
  typeIndexesIn = singleton

instance (Ord (o k), HasTypeIndexes o k t) => HasTypeIndexes o k (Map a t) where
  typeIndexesIn = Set.unions . fmap typeIndexesIn

instance (Ord (o k), HasTypeIndexes o k t) => HasTypeIndexes o k (Maybe t) where
  typeIndexesIn = Set.unions . fmap typeIndexesIn

instance (Ord (o k), HasTypeIndexes o k t) => HasTypeIndexes o k [t] where
  typeIndexesIn = Set.unions . fmap typeIndexesIn

instance (Ord (o k), HasTypeIndexes o k t) => HasTypeIndexes o k (NonEmpty t) where
  typeIndexesIn = Set.unions . fmap typeIndexesIn

instance (Ord (o k), HasTypeIndexes o k t) => HasTypeIndexes o k (Trait t) where
  typeIndexesIn = Set.unions . fmap typeIndexesIn

instance (HasTypeIndexes o k t) => HasTypeIndexes o k (Label t) where
  typeIndexesIn =
    \case
      Label t _ ->
        typeIndexesIn t

instance (Ord (o k), HasTypeIndexes o k (o k), HasTypeIndexes o k t) => HasTypeIndexes o k (Row o k t) where
  typeIndexesIn =
    \case
      Row.Extend _ t row ->
        typeIndexesIn t <> typeIndexesIn row
      Row.Variable t ->
        typeIndexesIn t
      Row.Nil ->
        mempty

instance (Ord (o k), HasTypeIndexes o k (o k)) => HasTypeIndexes o k (Type o k) where
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

instance (HasTypeIndexes o k t) => HasTypeIndexes o k (Pattern t) where
  typeIndexesIn =
    \case
      Pattern.Variable (Label t _) ->
        typeIndexesIn t

instance (Ord (o k), HasTypeIndexes o k t) => HasTypeIndexes o k (Scheme o k t) where
  typeIndexesIn =
    \case
      Forall qs ps t ->
        undefined

-- (typeIndexesIn t <> typeIndexesIn ps) \\. qs
-- (\\.) (typeIndexesIn t <> typeIndexesIn ps) qs
--        Set.filter (tork qs) (typeIndexesIn t <> typeIndexesIn ps)

----(\\.) :: Set (o k) -> Set (o k) -> Set (o k)
----(\\.) s1 s2 = Set.filter (fork s2) s1
--
-- tork :: Set a -> a -> Bool
-- tork = undefined
--
----fnork s2 s = tork s2 s
----
----fork s2 s = tork s2 s -- knork s `notElem` Set.map knork s2
----
----knork :: o k -> Int
----knork = undefined

instance HasTypeIndexes o k (MonomorphicSet (o k)) where
  typeIndexesIn = monomorphicSet
