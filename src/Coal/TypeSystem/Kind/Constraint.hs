{-# LANGUAGE StrictData #-}

module Coal.TypeSystem.Kind.Constraint (KindConstraint (..)) where

import Coal.Language.Type.Kind (Kind (..))

data KindConstraint = KEquality Kind Kind
  deriving (Show, Eq, Ord, Read)
