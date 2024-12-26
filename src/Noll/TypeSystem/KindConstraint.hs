{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.KindConstraint (
  KindConstraint (..),
  KindConstraintMetadata (..),
) where

data KindConstraint c k = KindEquality c k k
  deriving (Show, Eq, Ord, Read, Functor, Foldable)

data KindConstraintMetadata = KindConstraintMetadata
  deriving (Show, Eq, Ord, Read)
