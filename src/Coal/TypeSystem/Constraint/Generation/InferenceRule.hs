{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}

module Coal.TypeSystem.Constraint.Generation.InferenceRule (InferenceRule (..)) where

import Coal.Language (Kind (..), Scheme (..), Type (..), TypeIndex (..))
import Coal.TypeSystem.Substitution (Substitutable (..), applyT)
import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (transformBi)
import Extras (Name)

data InferenceRule k a
  = InferenceRulePlaceholder String
  | -- | Type annotation
    RuleAnnotation a (Type TypeIndex k) (Type TypeIndex k)
  | -- | Function application
    RuleApplication a (Type TypeIndex k) [Type TypeIndex k]
  | -- | If-condition must be of type bool
    RuleIfCondition a (Type TypeIndex k)
  | -- | The types of an if-expression's 'then' and 'else' branches must agree
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
    RuleUnaryOperator a
  | -- | TODO
    RuleBinaryOperator a
  | -- | TODO
    RuleTopLevelFunction a
  | -- | TODO
    RuleTopLevelConstant a
  | -- | TODO
    RuleTypeConstraint a Name (Type TypeIndex k) (Scheme TypeIndex k (Type TypeIndex k))
  | -- | TODO
    RuleDataConstructor a Name (Type TypeIndex k) (Scheme TypeIndex k (Type TypeIndex k))
  | -- | TODO
    RuleCodataRecord a (Type TypeIndex k) (Scheme TypeIndex k (Type TypeIndex k))
  | -- | TODO
    RuleUnfoldEquality a (Type TypeIndex k) (Type TypeIndex k)
  | -- | TODO
    RuleUnfoldExplicit a (Type TypeIndex k) (Scheme TypeIndex k (Type TypeIndex k))
  | -- | TODO
    RuleEntrypoint a (Type TypeIndex k)
  deriving (Show, Eq, Ord, Read, Data, Typeable)

instance (Data a) => Substitutable (InferenceRule Kind a) where
  apply = transformBi . applyT
