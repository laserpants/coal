{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Noll.Language.TypeIndexed (
  TypeIndexed (..),
  typeIdsIn,
  notBoundIn,
  freshIdIn,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set, singleton)
import qualified Data.Set as Set
import Noll.Label (Label (..))
import Noll.Language.Expression (Clause (..), Expression (..))
import Noll.Language.Expression.Binding (Binding (..))
import Noll.Language.Expression.Choice (Choice (..), Guard (..))
import Noll.Language.Pattern (Pattern (..))
import Noll.Language.Trait (Trait (..))
import Noll.Language.Type (Type (..), TypeIndex (..))
import Noll.Language.Type.Kind (Kind (..), KindIndex (..))
import Noll.Language.Type.Row (Row (..))
import Noll.Language.Type.Scheme (Scheme (..))
import Noll.Utils (unionMap)

class TypeIndexed k t | t -> k where
  typeIndexesIn :: t -> Set (TypeIndex k)

instance TypeIndexed k (TypeIndex k) where
  typeIndexesIn = singleton

instance (Ord k, TypeIndexed k t) => TypeIndexed k (Map a t) where
  typeIndexesIn = Set.unions . fmap typeIndexesIn

instance (Ord k, TypeIndexed k t) => TypeIndexed k (Maybe t) where
  typeIndexesIn = Set.unions . fmap typeIndexesIn

instance (Ord k, TypeIndexed k t) => TypeIndexed k [t] where
  typeIndexesIn = Set.unions . fmap typeIndexesIn

instance (Ord k, TypeIndexed k t) => TypeIndexed k (NonEmpty t) where
  typeIndexesIn = Set.unions . fmap typeIndexesIn

instance (Ord k, TypeIndexed k t) => TypeIndexed k (Trait t) where
  typeIndexesIn = Set.unions . fmap typeIndexesIn

instance (Ord k, TypeIndexed k t) => TypeIndexed k (Set t) where
  typeIndexesIn = Set.unions . Set.map typeIndexesIn

instance (TypeIndexed k t) => TypeIndexed k (Label t) where
  typeIndexesIn =
    \case
      Label t _ ->
        typeIndexesIn t

instance (Ord k, TypeIndexed k t) => TypeIndexed k (Row TypeIndex k t) where
  typeIndexesIn =
    \case
      RExtend _ t row ->
        typeIndexesIn t <> typeIndexesIn row
      RVariable t ->
        typeIndexesIn t
      RNil ->
        mempty

instance (Ord k) => TypeIndexed k (Type TypeIndex k) where
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

instance (Ord k, TypeIndexed k t) => TypeIndexed k (Pattern a t) where
  typeIndexesIn =
    \case
      PVariable _ (Label t _) ->
        typeIndexesIn t
      PConstructor _ (Label t _) ps ->
        typeIndexesIn t <> typeIndexesIn ps

instance (Ord k, TypeIndexed k t) => TypeIndexed k (Scheme TypeIndex k t) where
  typeIndexesIn =
    \case
      Forall qs ps t ->
        notBoundIn qs (typeIndexesIn t <> typeIndexesIn ps)

instance (Ord k) => TypeIndexed k (Binding Expression a (Type TypeIndex k)) where
  typeIndexesIn =
    \case
      BPattern _ p e ->
        typeIndexesIn p <> typeIndexesIn e

instance (Ord k) => TypeIndexed k (Guard Expression a (Type TypeIndex k)) where
  typeIndexesIn =
    \case
      CGuard e ->
        typeIndexesIn e

instance (Ord k) => TypeIndexed k (Choice Expression a (Type TypeIndex k)) where
  typeIndexesIn =
    \case
      CPlain _ gs e ->
        typeIndexesIn gs <> typeIndexesIn e

instance (Ord k) => TypeIndexed k (Clause Expression a (Type TypeIndex k)) where
  typeIndexesIn =
    \case
      EClause _ p cs ->
        typeIndexesIn p <> typeIndexesIn cs

instance (Ord k) => TypeIndexed k (Expression a (Type TypeIndex k)) where
  typeIndexesIn =
    \case
      EAnnotation _ e ->
        typeIndexesIn e
      EConstructor _ (Label t _) ->
        typeIndexesIn t
      EVariable _ (Label t _) ->
        typeIndexesIn t
      ELambda _ ps e ->
        typeIndexesIn ps <> typeIndexesIn e
      ELet _ gs e1 ->
        typeIndexesIn gs <> typeIndexesIn e1
      EIf _ t e1 e2 e3 ->
        typeIndexesIn t <> typeIndexesIn e1 <> typeIndexesIn e2 <> typeIndexesIn e3
      EApplication _ t e1 es ->
        typeIndexesIn t <> typeIndexesIn e1 <> typeIndexesIn es
      ELiteral{} ->
        mempty
      EMatch _ t e cs ->
        typeIndexesIn t <> typeIndexesIn e <> typeIndexesIn cs

class KindIndexed k where
  kindIndexesIn :: k -> Set KindIndex

instance (KindIndexed k) => KindIndexed [k] where
  kindIndexesIn = Set.unions . fmap kindIndexesIn

instance (KindIndexed k) => KindIndexed (NonEmpty k) where
  kindIndexesIn = Set.unions . fmap kindIndexesIn

instance (Ord k, KindIndexed k) => KindIndexed (Set k) where
  kindIndexesIn = Set.unions . Set.map kindIndexesIn

instance KindIndexed (Kind KindIndex) where
  kindIndexesIn =
    \case
      KVariable v ->
        Set.singleton v
      _ ->
        mempty

instance KindIndexed (TypeIndex (Kind KindIndex)) where
  kindIndexesIn =
    \case
      TypeIndex k _ ->
        kindIndexesIn k

instance KindIndexed KindIndex where
  kindIndexesIn = Set.singleton

instance KindIndexed (TypeIndex ()) where
  kindIndexesIn = mempty

notBoundIn :: Set (TypeIndex k) -> Set (TypeIndex k) -> Set (TypeIndex k)
notBoundIn s = Set.filter notBound
 where
  notBound index = typeIndexId index `notElem` Set.map typeIndexId s

typeIdsIn :: (TypeIndexed k t) => t -> Set Int
typeIdsIn = Set.map typeIndexId . typeIndexesIn

freshIdIn :: (Ord k, TypeIndexed k t) => t -> Int
freshIdIn t =
  if null typeIndexSet
    then 0
    else succ (maximum (typeIdsIn typeIndexSet))
 where
  typeIndexSet = typeIndexesIn t
