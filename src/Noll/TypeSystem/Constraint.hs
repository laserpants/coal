{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
