{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.TypeConstraint (
  TypeConstraintMetadata (..),
  MonomorphicSet (..),
  TypeConstraint (..),
  overMonomorphicSet,
)
where

import Data.Set (Set, intersection, union)
import Noll.Language (HasActive (..), Scheme (..), Type (..), TypeIndex (..), TypeIndexed (..))

-- | Monomorphic type variable set
newtype MonomorphicSet m = MonomorphicSet {monomorphicSet :: Set m}
  deriving (Show, Eq, Ord, Read, Semigroup, Monoid)

{-# INLINE overMonomorphicSet #-}
overMonomorphicSet :: (Set m -> Set m) -> MonomorphicSet m -> MonomorphicSet m
overMonomorphicSet fn MonomorphicSet{..} = MonomorphicSet{monomorphicSet = fn monomorphicSet}

data TypeConstraint c o k t
  = Equality c [t]
  | Implicit c t t (MonomorphicSet (o k))
  | Explicit c t (Scheme o k t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data TypeConstraintMetadata k a
  = TypeConstraintMetadata
  | -- | Function application
    ConstraintApplication a (Type TypeIndex k) (Type TypeIndex k)
  | -- | Type of if condition is bool
    ConstraintIfCondition a
  | -- | If expression 'then' and 'else' branches have identical types
    ConstraintIfBranches a (Type TypeIndex k) (Type TypeIndex k)
  | -- | Pattern guards are of type bool
    ConstraintMatchClauseGuard
  | ConstraintMatchClauseExpressions a
  | ConstraintMatchClausePatterns a
  deriving (Show, Eq, Ord, Read)

instance TypeIndexed k (MonomorphicSet (TypeIndex k)) where
  typeIndexesIn = monomorphicSet

instance (Ord k, TypeIndexed k t) => TypeIndexed k (TypeConstraint c TypeIndex k t) where
  typeIndexesIn =
    \case
      Equality _ ts ->
        typeIndexesIn ts
      Implicit _ t1 t2 m ->
        typeIndexesIn t1 <> typeIndexesIn t2 <> typeIndexesIn m
      Explicit _ t s ->
        typeIndexesIn t <> typeIndexesIn s

instance (Ord k, TypeIndexed k t) => HasActive k (TypeConstraint c TypeIndex k t) where
  activeIn =
    \case
      Equality _ ts ->
        typeIndexesIn ts
      Implicit _ t1 t2 m ->
        typeIndexesIn t1 `union` (typeIndexesIn t2 `intersection` typeIndexesIn m)
      Explicit _ t s ->
        typeIndexesIn t `union` typeIndexesIn s
