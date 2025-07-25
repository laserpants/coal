{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}

module Coal.TypeSystem.Constraint.Generation.InferenceRule (InferenceRule (..)) where

import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (transformBi)
import Extra (Name)
import Coal.Language (Kind (..), Type (..), TypeIndex (..))
import Coal.TypeSystem.Substitution (Substitutable (..), applyT)

data InferenceRule k a
  = InferenceRulePlaceholder
  | -- | Type annotation
    RuleAnnotation a (Type TypeIndex k) (Type TypeIndex k)
  | -- | Function application
    RuleApplication a (Type TypeIndex k) [Type TypeIndex k]
  | -- | Type of if-condition is bool
    RuleIfCondition a (Type TypeIndex k)
  | -- | If-expression 'then' and 'else' branches must have the same type
    RuleIfBranches a (Type TypeIndex k) (Type TypeIndex k)
  | -- Type of binding expression matches binding pattern
    RuleLetBindingPattern a (Type TypeIndex k) (Type TypeIndex k)
  | -- TODO
    RuleLetImplicit a Name (Type TypeIndex k) (Type TypeIndex k)
  | -- | Pattern guards are of type bool
    RuleMatchClauseGuard a
  | -- | Match clauses all have the same type as expression
    RuleMatchClauseExpressions a
  | -- | Match clause patterns have compatible types
    RuleMatchClausePatterns a
  | -- | TODO
    RuleBinaryOperator a
  | -- | TODO
    RuleTopLevelFunction a
  | -- | TODO
    RuleTopLevelConstant a
  deriving (Show, Eq, Ord, Read, Data, Typeable)

instance (Data a) => Substitutable (InferenceRule Kind a) where
  apply = transformBi . applyT
