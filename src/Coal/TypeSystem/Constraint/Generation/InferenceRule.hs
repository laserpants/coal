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
  = RuleAnnotation a (Type TypeIndex k) (Type TypeIndex k)
  | RuleApplication a (Type TypeIndex k) [Type TypeIndex k]
  | RuleIfCondition a (Type TypeIndex k)
  | RuleIfBranches a (Type TypeIndex k) (Type TypeIndex k)
  | RuleLetBindingPattern a (Type TypeIndex k) (Type TypeIndex k)
  | RuleLetImplicit a Name (Type TypeIndex k) (Type TypeIndex k)
  | RuleMatchClauseGuard a
  | RuleMatchClauseExpressions a
  | RuleTuple a (Type TypeIndex k) (Type TypeIndex k)
  | RuleListConstructor a (Type TypeIndex k) (Scheme TypeIndex k (Type TypeIndex k))
  | RuleListLiteral a [Type TypeIndex k]
  | RuleMatchClausePatterns a
  | RuleUnaryOperator a
  | RuleBinaryOperator a
  | RuleTopLevelFunction a
  | RuleTopLevelConstant a
  | RuleAsConstraint a
  | RuleFoldType a
  | RuleOrConstraint a [Type TypeIndex k]
  | RuleTypeConstraint a Name (Type TypeIndex k) (Scheme TypeIndex k (Type TypeIndex k))
  | RuleDataConstructor a Name (Type TypeIndex k) (Scheme TypeIndex k (Type TypeIndex k))
  | RuleCodataRecordExplicit a (Type TypeIndex k) (Scheme TypeIndex k (Type TypeIndex k))
  | RuleCodataRecordEquality a (Type TypeIndex k) (Type TypeIndex k)
  | RuleUnfoldEquality a Name (Type TypeIndex k) (Type TypeIndex k)
  | RuleUnfoldExplicit a (Type TypeIndex k) (Scheme TypeIndex k (Type TypeIndex k))
  | RuleSelectEquality a (Type TypeIndex k) (Type TypeIndex k)
  | RuleRecordEquality a (Type TypeIndex k) (Type TypeIndex k)
  | RuleRecordField a Name (Type TypeIndex k)
  | RuleRecordLacks a Name (Type TypeIndex k)
  | RuleTailRow a (Type TypeIndex k) (Type TypeIndex k)
  | RuleEntrypoint a (Type TypeIndex k)
  | RuleTraitInstance a (Type TypeIndex k) (Scheme TypeIndex k (Type TypeIndex k))
  | RuleAssumption a (Type TypeIndex k) (Type TypeIndex k)
  | RuleAssumptionExplicit a (Type TypeIndex k) (Scheme TypeIndex k (Type TypeIndex k))
  deriving (Show, Eq, Ord, Read, Data, Typeable)

instance (Data a) => Substitutable (InferenceRule Kind a) where
  apply = transformBi . applyT
