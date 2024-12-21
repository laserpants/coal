{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint.Collection where

import Control.Monad.RWS (MonadReader, MonadWriter, MonadState, MonadRWS, RWS)
import Noll.TypeSystem.Constraint (MonomorphicSet (..), TypeConstraint (..))

data ConstraintsContext o k = ConstraintsContext
  { contextMonomorphicSet :: MonomorphicSet o k
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE overContextMonomorphicSet #-}
overContextMonomorphicSet :: (MonomorphicSet o k -> MonomorphicSet o k) -> ConstraintsContext o k -> ConstraintsContext o k
overContextMonomorphicSet fn ConstraintsContext{..} = ConstraintsContext{contextMonomorphicSet = fn contextMonomorphicSet, ..}

newtype Constraints o k t a = Constraints {constraintsMonad :: RWS (ConstraintsContext o k) [TypeConstraint o k t] () a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (ConstraintsContext o k)
    , MonadWriter [TypeConstraint o k t]
    , MonadState ()
    , MonadRWS (ConstraintsContext o k) [TypeConstraint o k t] ()
    )
