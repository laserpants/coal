{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint.Collect where

import Noll.TypeSystem.Constraint (MonomorphicSet (..))

data ConstraintsContext o k = ConstraintsContext
  { contextMonomorphicSet :: MonomorphicSet o k
  }
  deriving (Show, Eq, Ord, Read)
