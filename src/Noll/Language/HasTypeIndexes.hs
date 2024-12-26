{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Noll.Language.HasTypeIndexes (
  HasTypeIndexes (..),
  typeIdsIn,
  notBoundIn,
  freshIdIn,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set, singleton)
import qualified Data.Set as Set
import Noll.Label (Label (..))
import Noll.Language.Expression (Expression (..))
import Noll.Language.Expression.Binding (Binding (..))
import Noll.Language.Pattern (Pattern (..))
import Noll.Language.Trait (Trait (..))
import Noll.Language.Type (Type (..), TypeIndex (..))
import Noll.Language.Type.Row (Row (..))
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
      RExtend _ t row ->
        typeIndexesIn t <> typeIndexesIn row
      RVariable t ->
        typeIndexesIn t
      RNil ->
        mempty

instance (Ord k) => HasTypeIndexes k (Type TypeIndex k) where
  typeIndexesIn =
    \case
      TApplication _ t ts ->
        typeIndexesIn t <> typeIndexesIn ts
      TArrow t1 t2 ->
        typeIndexesIn t1 <> typeIndexesIn t2
      TConstructor{} ->
        mempty
      TIntrinsic{} ->
        mempty
      TRow row ->
        typeIndexesIn row
      TVariable t ->
        typeIndexesIn t
      TAlias _ _ t ->
        typeIndexesIn t

instance (HasTypeIndexes k t) => HasTypeIndexes k (Pattern t) where
  typeIndexesIn =
    \case
      PVariable (Label t _) ->
        typeIndexesIn t

instance (Ord k, HasTypeIndexes k t) => HasTypeIndexes k (Scheme TypeIndex k t) where
  typeIndexesIn =
    \case
      Forall qs ps t ->
        notBoundIn qs (typeIndexesIn t <> typeIndexesIn ps)

instance (Ord k) => HasTypeIndexes k (Binding Expression (Type TypeIndex k)) where
  typeIndexesIn =
    \case
      BPattern p e ->
        typeIndexesIn p <> typeIndexesIn e

instance (Ord k) => HasTypeIndexes k (Expression (Type TypeIndex k)) where
  typeIndexesIn =
    \case
      EConstructor (Label t _) ->
        typeIndexesIn t
      EVariable (Label t _) ->
        typeIndexesIn t
      ELambda ps e ->
        typeIndexesIn ps <> typeIndexesIn e
      ELet gs e1 ->
        typeIndexesIn gs <> typeIndexesIn e1
      EIf e1 e2 e3 ->
        typeIndexesIn e1 <> typeIndexesIn e2 <> typeIndexesIn e3
      EApplication t e1 es ->
        typeIndexesIn t <> typeIndexesIn e1 <> typeIndexesIn es
      ELiteral{} ->
        mempty

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
