{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint.Collection where

import Noll.TypeSystem.Constraint (MonomorphicSet (..))

data ConstraintsContext o k = ConstraintsContext
  { contextMonomorphicSet :: MonomorphicSet o k
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE overContextMonomorphicSet #-}
overContextMonomorphicSet :: (MonomorphicSet v k -> MonomorphicSet v k) -> ConstraintsContext v k -> ConstraintsContext v k
overContextMonomorphicSet fn ConstraintsContext{..} = ConstraintsContext{contextMonomorphicSet = fn contextMonomorphicSet, ..}
