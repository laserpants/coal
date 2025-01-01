{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.KindConstraint.Rule (KindRule (..)) where

import Noll.Language (OpaqueType (..))
import Noll.TypeSystem.KindSubstitution (KindSubstitutable (..))

data KindRule
  = KindRule
  | RuleTypeApplication OpaqueType [OpaqueType]
  deriving (Show, Eq, Ord, Read)

instance KindSubstitutable KindRule where
  applyKindSub sub =
    \case
      KindRule ->
        KindRule
      RuleTypeApplication t ts ->
        RuleTypeApplication t ts
