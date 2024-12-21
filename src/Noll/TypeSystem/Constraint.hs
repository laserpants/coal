{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint (MonomorphicSet (..), TypeConstraint (..)) where

import Data.Set (Set)
import Noll.Language.Type.Scheme (Scheme (..))

-- | Monomorphic type variable set
newtype MonomorphicSet o k = MonomorphicSet {monomorphicSet :: Set (o k)}
  deriving (Show, Eq, Ord, Read, Semigroup, Monoid)

data TypeConstraint o k t
  = Equality t t
  | Implicit t t (MonomorphicSet o k)
  | Explicit t (Scheme o k t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
