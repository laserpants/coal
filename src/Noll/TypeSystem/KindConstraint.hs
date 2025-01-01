{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.KindConstraint (
  KindConstraint (..),
  KindRule (..),
) where

import Noll.Language (OpaqueType (..))
import Noll.TypeSystem.KindConstraint.Rule (KindRule (..))

data KindConstraint c k = KindEquality c k k
  deriving (Show, Eq, Ord, Read, Functor, Foldable)
