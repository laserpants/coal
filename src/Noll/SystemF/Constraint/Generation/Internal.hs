{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.SystemF.Constraint.Generation.Internal (
  TypeAnnotationError,
  ConstraintsGenerationError (..),
  InferenceRule (..),
  TypeAnnotationError (..),
  ConstraintsGenerationContext (..),
  ConstraintsGenerationStack (..),
  ConstraintsGenerationOutput (..),
  ConstraintsGenerationState (..),
  monosetInsert,
  monosetInsertMany,
  localMonoset,
  runConstraintsGenerationStack,
  evalConstraintsGenerationStack,
  overConstraintsGenerationStateTypeIndexes,
  updateConstraintsGenerationSupply,
) where

import Control.Monad.RWS (
  MonadRWS,
  MonadReader,
  MonadState,
  MonadWriter,
  RWS,
  evalRWS,
  gets,
  local,
  modify,
  runRWS,
 )
import Noll.Common.Environment (Environment (..))
import Noll.Common.Supply (Supply (..))
import Noll.Language (Constructor (..), Kind (..), Type (..), TypeIndex (..))
import Noll.SystemF.Constraint (
  Constraint (..),
  MonomorphicSet (..),
  overMonomorphicSet,
 )
import Noll.SystemF.Substitution (Substitutable (..))
import Noll.Utils (Dictionary, Name)

import qualified Data.Set as Set

data InferenceRule k a
  = InferenceRule Int
  | -- | Type annotation
    InferAnnotation a (Type TypeIndex k)
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
      InferBinaryOperator a ->
        InferBinaryOperator a

data TypeAnnotationError a
  = -- Kind error
    KindMismatch a
  | -- | Type constructor is not in scope
    NoTypeConstructor a Name
  | -- | Two or more named parameters refer to the same inferred type variable.
    -- E.g., the annotation reads (a -> b) -> c -> b, but the function is
    -- fn(f, x) => f(x), which forces 'a' and 'c' to be the same type.
    -- The type signature claims that the function is polymorphic with respect
    -- to any choice of variables a, b, and c, and is therefore incorrect.
    NonDistinctTypeParameters [[(Name, a)]]
  | -- | Type parameter resolves to a concrete type; e.g., fn(x : a, y : int32) => x + y
    ResolvesToMonomorphicType Name (Type TypeIndex Kind)
  deriving (Show, Eq, Ord, Read)

data ConstraintsGenerationError a
  = NoDataConstructor a Name
  | DataConstructorArityMismatch a Name Int Int
  | IllFormedTypeAnnotation (TypeAnnotationError a)
  deriving (Show, Eq, Ord, Read)

data ConstraintsGenerationContext o k t = ConstraintsGenerationContext
  { constraintsGenerationContextMonomorphicSet :: MonomorphicSet (o k)
  , constraintsGenerationContextDataConstructorEnv :: Environment (Constructor o k t)
  , constraintsGenerationContextTypeConstructorEnv :: Environment k
  }
  deriving (Show, Eq, Ord, Read)

data ConstraintsGenerationState c = ConstraintsGenerationState
  { constraintsGenerationStateTypeIndexes :: Dictionary (c, TypeIndex Kind)
  , constraintsGenerationStateSupply :: Int
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE overConstraintsGenerationStateTypeIndexes #-}
overConstraintsGenerationStateTypeIndexes :: (Dictionary (c, TypeIndex Kind) -> Dictionary (c, TypeIndex Kind)) -> ConstraintsGenerationState c -> ConstraintsGenerationState c
overConstraintsGenerationStateTypeIndexes fn ConstraintsGenerationState{..} = ConstraintsGenerationState{constraintsGenerationStateTypeIndexes = fn constraintsGenerationStateTypeIndexes, ..}

{-# INLINE overConstraintsGenerationStateSupply #-}
overConstraintsGenerationStateSupply :: (Int -> Int) -> ConstraintsGenerationState c -> ConstraintsGenerationState c
overConstraintsGenerationStateSupply fn ConstraintsGenerationState{..} = ConstraintsGenerationState{constraintsGenerationStateSupply = fn constraintsGenerationStateSupply, ..}

instance Supply (ConstraintsGenerationState c) where
  updateSupply = overConstraintsGenerationStateSupply
  getSupply = constraintsGenerationStateSupply

type ConstraintsGenerationOutput c o k t = Either (ConstraintsGenerationError c) (Constraint (InferenceRule k c) o k t)

type ConstraintsGenerationMonad c o k t = RWS (ConstraintsGenerationContext o k t) [ConstraintsGenerationOutput c o k t] (ConstraintsGenerationState c)

newtype ConstraintsGenerationStack c o k t a = ConstraintsGenerationStack {constraintsGenerationMonad :: ConstraintsGenerationMonad c o k t a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (ConstraintsGenerationContext o k t)
    , MonadWriter [ConstraintsGenerationOutput c o k t]
    , MonadState (ConstraintsGenerationState c)
    , MonadRWS (ConstraintsGenerationContext o k t) [ConstraintsGenerationOutput c o k t] (ConstraintsGenerationState c)
    )

{-# INLINE evalConstraintsGenerationStack #-}
evalConstraintsGenerationStack :: Int -> ConstraintsGenerationContext o k t -> ConstraintsGenerationStack c o k t a -> (a, [ConstraintsGenerationOutput c o k t])
evalConstraintsGenerationStack supply ctx a = evalRWS (constraintsGenerationMonad a) ctx (ConstraintsGenerationState mempty supply)

{-# INLINE runConstraintsGenerationStack #-}
runConstraintsGenerationStack :: Int -> ConstraintsGenerationContext o k t -> ConstraintsGenerationStack c o k t a -> (a, ConstraintsGenerationState c, [ConstraintsGenerationOutput c o k t])
runConstraintsGenerationStack supply ctx a = runRWS (constraintsGenerationMonad a) ctx (ConstraintsGenerationState mempty supply)

{-# INLINE updateConstraintsGenerationSupply #-}
updateConstraintsGenerationSupply :: Int -> ConstraintsGenerationStack c o k t ()
updateConstraintsGenerationSupply supply = modify (overConstraintsGenerationStateSupply (const supply))

{-# INLINE getConstraintsGenerationSupply #-}
getConstraintsGenerationSupply :: ConstraintsGenerationStack c o k t Int
getConstraintsGenerationSupply = gets constraintsGenerationStateSupply

{-# INLINE overConstraintsGenerationMonomorphicSet #-}
overConstraintsGenerationMonomorphicSet :: (MonomorphicSet (o k) -> MonomorphicSet (o k)) -> ConstraintsGenerationContext o k t -> ConstraintsGenerationContext o k t
overConstraintsGenerationMonomorphicSet fn ConstraintsGenerationContext{..} = ConstraintsGenerationContext{constraintsGenerationContextMonomorphicSet = fn constraintsGenerationContextMonomorphicSet, ..}

{-# INLINE monosetInsert #-}
monosetInsert :: (Ord k) => TypeIndex k -> MonomorphicSet (TypeIndex k) -> MonomorphicSet (TypeIndex k)
monosetInsert = overMonomorphicSet . Set.insert

{-# INLINE monosetInsertMany #-}
monosetInsertMany :: (Ord k, Foldable f) => f (TypeIndex k) -> MonomorphicSet (TypeIndex k) -> MonomorphicSet (TypeIndex k)
monosetInsertMany = flip (foldr monosetInsert)

{-# INLINE localMonoset #-}
localMonoset :: (MonomorphicSet (o k) -> MonomorphicSet (o k)) -> ConstraintsGenerationStack c o k t a -> ConstraintsGenerationStack c o k t a
localMonoset = local . overConstraintsGenerationMonomorphicSet
