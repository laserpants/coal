{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint (
  MonomorphicSet (..),
  TypeConstraint (..),
  overMonomorphicSet,
)
where

import Data.Set (Set)
import Noll.Language.HasTypeIndexes (HasTypeIndexes (..))
import Noll.Language.Type.Index (TypeIndex (..))
import Noll.Language.Type.Scheme (Scheme (..))

-- | Monomorphic type variable set
newtype MonomorphicSet m = MonomorphicSet {monomorphicSet :: Set m}
  deriving (Show, Eq, Ord, Read, Semigroup, Monoid)

{-# INLINE overMonomorphicSet #-}
overMonomorphicSet :: (Set m -> Set m) -> MonomorphicSet m -> MonomorphicSet m
overMonomorphicSet fn MonomorphicSet{..} = MonomorphicSet{monomorphicSet = fn monomorphicSet}

data TypeConstraint o k t
  = Equality t t
  | Implicit t t (MonomorphicSet (o k))
  | Explicit t (Scheme o k t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

instance HasTypeIndexes k (MonomorphicSet (TypeIndex k)) where
  typeIndexesIn = monomorphicSet

instance (Ord k, HasTypeIndexes k t) => HasTypeIndexes k (TypeConstraint TypeIndex k t) where
  typeIndexesIn =
    \case
      Equality t1 t2 ->
        typeIndexesIn t1 <> typeIndexesIn t2
      Implicit t1 t2 m ->
        typeIndexesIn t1 <> typeIndexesIn t2 <> typeIndexesIn m
      Explicit t s ->
        typeIndexesIn t <> typeIndexesIn s
