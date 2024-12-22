{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint.Collection (
  ConstraintsContext (..),
  Constraints (..),
)
where

import Control.Monad.RWS (MonadRWS, MonadReader, MonadState, MonadWriter, RWS)
import Noll.Language.Expression (Expression (..))
import Noll.Language.Type (Type (..))
import Noll.Language.Type.Index (TypeIndex (..))
import Noll.Language.Type.Opaque (OpaqueType)
import Noll.TypeSystem.Assumption (Assumption (..))
import Noll.TypeSystem.Constraint (MonomorphicSet (..), TypeConstraint (..))

data ConstraintsContext o k = ConstraintsContext
  { contextMonomorphicSet :: MonomorphicSet (o k)
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE overContextMonomorphicSet #-}
overContextMonomorphicSet :: (MonomorphicSet (o k) -> MonomorphicSet (o k)) -> ConstraintsContext o k -> ConstraintsContext o k
overContextMonomorphicSet fn ConstraintsContext{..} = ConstraintsContext{contextMonomorphicSet = fn contextMonomorphicSet, ..}

type ConstraintsMonad o k t = RWS (ConstraintsContext o k) [TypeConstraint o k t] ()

newtype Constraints o k t a = Constraints {constraintsMonad :: ConstraintsMonad o k t a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (ConstraintsContext o k)
    , MonadWriter [TypeConstraint o k t]
    , MonadState ()
    , MonadRWS (ConstraintsContext o k) [TypeConstraint o k t] ()
    )

collectConstraints ::
  Expression OpaqueType ->
  Constraints TypeIndex () OpaqueType ([Assumption OpaqueType], Expression OpaqueType)
collectConstraints =
  undefined
