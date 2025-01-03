{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint.Rule (
  InferenceRule (..),
  Assumption (..),
  assumptionNameIs,
) where

import Noll.Language (Kind (..), Scheme (..), Type (..), TypeIndex (..))
import Noll.TypeSystem.Substitution (Substitutable (..))
import Noll.Utils (Name)

data InferenceRule k a
  = InferenceRule Int
  | -- | Type annotation
    InferAnnotation a (Scheme TypeIndex Kind (Type TypeIndex Kind))
  | -- | Function application
    InferApplication a (Type TypeIndex k) [Type TypeIndex k]
  | -- | Type of if condition is bool
    InferIfCondition a (Type TypeIndex k)
  | -- | If expression 'then' and 'else' branches have identical types
    InferIfBranches a (Type TypeIndex k) (Type TypeIndex k)
  | -- Let type of body matches binding pattern
    InferLetBindingPattern a (Type TypeIndex k) (Type TypeIndex k)
  | -- TODO
    InferLetImplicit a Name (Type TypeIndex k) (Type TypeIndex k)
  | -- | Pattern guards are of type bool
    InferMatchClauseGuard
  | -- | Match clauses all have the same type as expression
    InferMatchClauseExpressions a
  | -- | Match clause patterns have identical types
    InferMatchClausePatterns a
  deriving (Show, Eq, Ord, Read)

instance Substitutable (InferenceRule Kind c) where
  apply sub =
    \case
      InferenceRule n ->
        InferenceRule n
      InferAnnotation a s ->
        InferAnnotation a s
      InferApplication a t ts ->
        InferApplication a (apply sub t) (apply sub ts)
      InferIfCondition a t ->
        InferIfCondition a (apply sub t)
      InferIfBranches a t1 t2 ->
        InferIfBranches a (apply sub t1) (apply sub t2)
      InferLetBindingPattern a t1 t2 ->
        InferLetBindingPattern a (apply sub t1) (apply sub t2)
      InferLetImplicit a name t1 t2 ->
        InferLetImplicit a name (apply sub t1) (apply sub t2)
      InferMatchClauseGuard ->
        InferMatchClauseGuard
      InferMatchClauseExpressions a ->
        InferMatchClauseExpressions a
      InferMatchClausePatterns a ->
        InferMatchClausePatterns a

data Assumption t = Assumption
  { assumptionName :: Name
  , assumptionType :: t
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE assumptionNameIs #-}
assumptionNameIs :: Name -> Assumption t -> Bool
assumptionNameIs name Assumption{..} = assumptionName == name
