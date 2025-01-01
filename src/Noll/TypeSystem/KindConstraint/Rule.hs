{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.KindConstraint.Rule (KindRule (..)) where

import Noll.Language (OpaqueType (..))

data KindRule
  = KindRule
  | RuleTypeApplication OpaqueType [OpaqueType]
  deriving (Show, Eq, Ord, Read)
