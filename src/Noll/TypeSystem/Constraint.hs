{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint where

import Data.Set (Set)
import Noll.Language.Type.Scheme (Scheme (..))

-- | Monomorphic type variable set
newtype Monomorphic o k = Monomorphic {monomorphicSet :: Set (o k)}
  deriving (Show, Eq, Ord, Read, Semigroup, Monoid)

data TypeConstraint o k t
  = EqualityConstraint t t
  | ImplicitConstraint t t (Monomorphic o k)
  | ExplicitConstraint t (Scheme o k t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
