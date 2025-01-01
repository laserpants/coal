{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.KindConstraint (
  KindConstraint (..),
) where

data KindConstraint c k = KindEquality c k k
  deriving (Show, Eq, Ord, Read, Functor, Foldable)
