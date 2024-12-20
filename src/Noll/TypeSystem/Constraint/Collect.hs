{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint.Collect where

import Noll.TypeSystem.Constraint (Monomorphic (..))

data ConstraintsContext o k = ConstraintsContext
  { contextMonoset :: Monomorphic o k
  }
  deriving (Show, Eq, Ord, Read)
