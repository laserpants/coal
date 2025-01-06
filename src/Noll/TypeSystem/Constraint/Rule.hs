{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint.Rule (
  InferenceRule (..),
  Assumption (..),
  assumptionNameIs,
  assumptionNameIsOneOf,
  assumptionNameIsNotOneOf,
) where

import Noll.Language (Kind (..), Scheme (..), Type (..), TypeIndex (..))
import Noll.TypeSystem.Substitution (Substitutable (..))
import Noll.Utils (Name)

data InferenceRule k a
  = InferenceRule Int
  | -- | Type annotation
    InferAnnotation a (Type TypeIndex k)
  | -- | Function application
    InferApplication a (Type TypeIndex k) [Type TypeIndex k]
  | -- | Type of if condition is bool
    InferIfCondition a (Type TypeIndex k)
  | -- | If expression 'then' and 'else' branches must have the same type
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
  deriving (Show, Eq, Ord, Read)

instance Substitutable (InferenceRule Kind a) where
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
      InferMatchClauseGuard a ->
        InferMatchClauseGuard a
      InferMatchClauseExpressions a ->
        InferMatchClauseExpressions a
      InferMatchClausePatterns a ->
        InferMatchClausePatterns a

data Assumption t = Assumption
  { assumptionName :: Name
  , assumptionType :: t
  }
  deriving (Show, Eq, Ord, Read)

instance (Substitutable t) => Substitutable (Assumption t) where
  apply sub (Assumption name t) =
    Assumption name (apply sub t)

{-# INLINE assumptionNameIs #-}
assumptionNameIs :: Name -> Assumption t -> Bool
assumptionNameIs name Assumption{..} = assumptionName == name

{-# INLINE assumptionNameIsOneOf #-}
assumptionNameIsOneOf :: [Name] -> Assumption t -> Bool
assumptionNameIsOneOf names Assumption{..} = assumptionName `elem` names

{-# INLINE assumptionNameIsNotOneOf #-}
assumptionNameIsNotOneOf :: [Name] -> Assumption t -> Bool
assumptionNameIsNotOneOf names Assumption{..} = assumptionName `notElem` names
