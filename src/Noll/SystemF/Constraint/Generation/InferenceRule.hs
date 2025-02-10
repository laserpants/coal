{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}

module Noll.SystemF.Constraint.Generation.InferenceRule (InferenceRule (..)) where

import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (transformBi)
import Noll.Language (Kind (..), Type (..), TypeIndex (..))
import Noll.SystemF.Substitution (Substitutable (..), applyT)
import Noll.Utils (Name)

data InferenceRule k a
  = InferenceRule Int
  | -- | Type annotation
    InferAnnotation a (Type TypeIndex k) (Type TypeIndex k)
  | -- | Function application
    InferApplication a (Type TypeIndex k) [Type TypeIndex k]
  | -- | Type of if-condition is bool
    InferIfCondition a (Type TypeIndex k)
  | -- | If-expression 'then' and 'else' branches must have the same type
    InferIfBranches a (Type TypeIndex k) (Type TypeIndex k)
  | -- Type of binding expression matches binding pattern
    InferLetBindingPattern a (Type TypeIndex k) (Type TypeIndex k)
  | -- TODO
    InferLetImplicit a Name (Type TypeIndex k) (Type TypeIndex k)
  | -- | Pattern guards are of type bool
    InferMatchClauseGuard a
  | -- | Match clauses all have the same type as expression
    InferMatchClauseExpressions a
  | -- | Match clause patterns have identical types
    InferMatchClausePatterns a
  | -- | TODO
    InferBinaryOperator a
  | -- | TODO
    InferTopLevelFunction a
  | -- | TODO
    InferTopLevelConstant a
  deriving (Show, Eq, Ord, Read, Data, Typeable)

instance (Data a) => Substitutable (InferenceRule Kind a) where
  apply = transformBi . applyT
