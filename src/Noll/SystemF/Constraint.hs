{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.SystemF.Constraint (
  MonomorphicSet (..),
  Constraint (..),
  overMonomorphicSet,
) where

import Data.Set (Set, intersection, union)
import Noll.Language (
  HasActive (..),
  Scheme (..),
  TypeIndex (..),
  TypeIndexed (..),
 )

-- | Monomorphic type variable set
newtype MonomorphicSet m = MonomorphicSet {monomorphicSet :: Set m}
  deriving (Show, Eq, Ord, Read, Semigroup, Monoid)

{-# INLINE overMonomorphicSet #-}
overMonomorphicSet :: (Set m -> Set m) -> MonomorphicSet m -> MonomorphicSet m
overMonomorphicSet fn MonomorphicSet{..} = MonomorphicSet{monomorphicSet = fn monomorphicSet}

data Constraint c o k t
  = Equality c [t]
  | Implicit c t t (MonomorphicSet (o k))
  | Explicit c t (Scheme o k t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

instance TypeIndexed k (MonomorphicSet (TypeIndex k)) where
  typeIndexesIn = monomorphicSet

instance (Ord k, TypeIndexed k t) => TypeIndexed k (Constraint c TypeIndex k t) where
  typeIndexesIn =
    \case
      Equality _ ts ->
        typeIndexesIn ts
      Implicit _ t1 t2 m ->
        typeIndexesIn t1 <> typeIndexesIn t2 <> typeIndexesIn m
      Explicit _ t s ->
        typeIndexesIn t <> typeIndexesIn s

instance (Ord k, TypeIndexed k t) => HasActive k (Constraint c TypeIndex k t) where
  activeIn =
    \case
      Equality _ ts ->
        typeIndexesIn ts
      Implicit _ t1 t2 m ->
        typeIndexesIn t1 `union` (typeIndexesIn t2 `intersection` typeIndexesIn m)
      Explicit _ t s ->
        typeIndexesIn t `union` typeIndexesIn s
