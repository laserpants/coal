{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.Language.Op (Op (..)) where

-- | Binary operators
data Op a
  = -- | Equality
    OEqInt32 a a
  | OEqInt64 a a
  | -- | Inequality
    ONEqInt32 a a
  | ONEqInt64 a a
  | -- | Less than
    OLtInt32 a a
  | OLtInt64 a a
  | -- | Greater than
    OGtInt32 a a
  | OGtInt64 a a
  | -- | Less than or equal to
    OLtEInt32 a a
  | OLtEInt64 a a
  | -- | Greater than or equal to
    OGtEInt32 a a
  | OGtEInt64 a a
  | -- | Addition
    OAddInt32 a a
  | OAddInt64 a a
  | -- | Subtraction
    OSubInt32 a a
  | OSubInt64 a a
  | -- | Multiplication
    OMulInt32 a a
  | OMulInt64 a a
  | OMulFloat a a
  | OMulDouble a a
  | -- | Division
    ODivInt32 a a
  | ODivInt64 a a
  | ODivFloat a a
  | ODivDouble a a
  | -- | Logical OR
    OOr a a
  | -- | Logical AND
    OAnd a a
  | -- | Logical NOT
    ONot a
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
