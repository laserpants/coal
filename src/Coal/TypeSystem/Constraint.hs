{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Coal.TypeSystem.Constraint (
  Monomorphic (..),
  Constraint (..),
  overMonomorphicSet,
) where

import Coal.Language (HasActive (..), Scheme (..), TypeIndex (..), TypeIndexed (..))
import Data.Data (Data, Typeable)
import Data.Set (Set, intersection, union)
import Extra (Name, Over)

-- | Monomorphic type variable set
newtype Monomorphic m = Monomorphic {monomorphicSet :: Set m}
  deriving (Show, Eq, Ord, Read, Semigroup, Monoid, Data, Typeable)

{-# INLINE overMonomorphicSet #-}
overMonomorphicSet :: Over (Monomorphic m) (Set m)
overMonomorphicSet fn (Monomorphic s) = Monomorphic (fn s)

data Constraint c o a t
  = Equality c [t]
  | Implicit c t t (Monomorphic (o a))
  | Explicit c t (Scheme o a t)
  | Lacks c t Name
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)

instance TypeIndexed k (Monomorphic (TypeIndex k)) where
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
      Lacks{} ->
        mempty

instance (Ord k, TypeIndexed k t) => HasActive k (Constraint c TypeIndex k t) where
  activeIn =
    \case
      Equality _ ts ->
        typeIndexesIn ts
      Implicit _ t1 t2 m ->
        typeIndexesIn t1 `union` (typeIndexesIn t2 `intersection` typeIndexesIn m)
      Explicit _ t s ->
        typeIndexesIn t `union` typeIndexesIn s
      Lacks{} ->
        mempty
