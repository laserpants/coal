{-# LANGUAGE StrictData #-}

module Coal.ProtoTypeSystem.Kind.Constraint (ProtoKindConstraint (..)) where

import Coal.Language.Type.Kind (Kind (..))

data ProtoKindConstraint = ProtoKEquality Kind Kind
  deriving (Show, Eq, Ord, Read)
