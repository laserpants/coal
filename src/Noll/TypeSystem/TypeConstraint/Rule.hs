{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.TypeConstraint.Rule (TypeRule (..)) where

import Noll.Language (Type, TypeIndex)

data TypeRule k a
  = TypeRule
  | -- | Function application
    RuleApplication a (Type TypeIndex k) [Type TypeIndex k]
  | -- | Type of if condition is bool
    RuleIfCondition a
  | -- | If expression 'then' and 'else' branches have identical types
    RuleIfBranches a (Type TypeIndex k) (Type TypeIndex k)
  | -- | Pattern guards are of type bool
    RuleMatchClauseGuard
  | -- | Match clauses all have the same type as expression
    RuleMatchClauseExpressions a
  | -- | Match clause patterns have identical types
    RuleMatchClausePatterns a
  deriving (Show, Eq, Ord, Read)
