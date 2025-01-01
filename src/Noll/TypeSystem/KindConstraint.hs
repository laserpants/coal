{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.KindConstraint (
  KindConstraint (..),
  KindConstraintMetadata (..),
) where

import Noll.Language (OpaqueType (..))

data KindConstraint c k = KindEquality c k k
  deriving (Show, Eq, Ord, Read, Functor, Foldable)

data KindConstraintMetadata
  = KindConstraintMetadata
  | RuleTypeApplication OpaqueType [OpaqueType]
  deriving (Show, Eq, Ord, Read)
