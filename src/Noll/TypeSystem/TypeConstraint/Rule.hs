{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.TypeConstraint.Rule (TypeRule (..)) where

import Noll.Language (Type, TypeIndex)
import Noll.TypeSystem.TypeSubstitution (TypeSubstitutable (..))

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

instance TypeSubstitutable (TypeRule () c) where
  apply sub =
    \case
      TypeRule ->
        TypeRule
      RuleApplication a t ts ->
        RuleApplication a (apply sub t) (apply sub ts)
      RuleIfCondition a ->
        RuleIfCondition a
      RuleIfBranches a t1 t2 ->
        RuleIfBranches a (apply sub t1) (apply sub t2)
      RuleMatchClauseGuard ->
        RuleMatchClauseGuard
      RuleMatchClauseExpressions a ->
        RuleMatchClauseExpressions a
      RuleMatchClausePatterns a ->
        RuleMatchClausePatterns a
