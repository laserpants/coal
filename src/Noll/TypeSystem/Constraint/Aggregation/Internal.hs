{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint.Aggregation.Internal (
  TypeAnnotationError,
  ConstraintsGenerationError (..),
  InferenceRule (..),
  TypeAnnotationError (..),
  ConstraintsGenerationContext (..),
  AggregationStack (..),
  AggregationOutput (..),
  monosetInsert,
  monosetInsertMany,
  localMonoset,
  runAggregationStack,
  evalAggregationStack,
) where

import Control.Monad.RWS (
  MonadRWS,
  MonadReader,
  MonadState,
  MonadWriter,
  RWS,
  evalRWS,
  local,
  runRWS,
 )
import Noll.Language (Constructor (..), Kind (..), Type (..), TypeIndex (..))
import Noll.Library.Environment (Environment (..))
import Noll.TypeSystem.Constraint (
  Constraint (..),
  MonomorphicSet (..),
  overMonomorphicSet,
 )
import Noll.TypeSystem.Substitution (Substitutable (..))
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

data TypeAnnotationError a
  = -- Kind error
    KindMismatch a
  | -- | Type constructor is not in scope
    NoTypeConstructor a Name
  | -- | Two or more named parameters refer to the same inferred type variable.
    -- E.g., the annotation reads (a -> b) -> c -> b, but the function is
    -- fn(f, x) => f(x), which requires 'a' and 'c' to be the same type.
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
  { aggregationMonomorphicSet :: MonomorphicSet (o k)
  , aggregationDataConstructorEnv :: Environment (Constructor o k t)
  , aggregationTypeConstructorEnv :: Environment k
  , aggregationIndexTreshold :: Int
  }
  deriving (Show, Eq, Ord, Read)

type AggregationOutput c o k t = Either (ConstraintsGenerationError c) (Constraint (InferenceRule k c) o k t)

type AggregationMonad c o k t = RWS (ConstraintsGenerationContext o k t) [AggregationOutput c o k t] (Dictionary (c, TypeIndex Kind))

newtype AggregationStack c o k t a = AggregationStack {aggregationMonad :: AggregationMonad c o k t a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (ConstraintsGenerationContext o k t)
    , MonadWriter [AggregationOutput c o k t]
    , MonadState (Dictionary (c, TypeIndex Kind))
    , MonadRWS (ConstraintsGenerationContext o k t) [AggregationOutput c o k t] (Dictionary (c, TypeIndex Kind))
    )

{-# INLINE evalAggregationStack #-}
evalAggregationStack :: ConstraintsGenerationContext o k t -> AggregationStack c o k t a -> (a, [AggregationOutput c o k t])
evalAggregationStack ctx a = evalRWS (aggregationMonad a) ctx mempty

{-# INLINE runAggregationStack #-}
runAggregationStack :: ConstraintsGenerationContext o k t -> AggregationStack c o k t a -> (a, Dictionary (c, TypeIndex Kind), [AggregationOutput c o k t])
runAggregationStack ctx a = runRWS (aggregationMonad a) ctx mempty

{-# INLINE overAggregationMonomorphicSet #-}
overAggregationMonomorphicSet :: (MonomorphicSet (o k) -> MonomorphicSet (o k)) -> ConstraintsGenerationContext o k t -> ConstraintsGenerationContext o k t
overAggregationMonomorphicSet fn ConstraintsGenerationContext{..} = ConstraintsGenerationContext{aggregationMonomorphicSet = fn aggregationMonomorphicSet, ..}

{-# INLINE monosetInsert #-}
monosetInsert :: (Ord k) => TypeIndex k -> MonomorphicSet (TypeIndex k) -> MonomorphicSet (TypeIndex k)
monosetInsert = overMonomorphicSet . Set.insert

{-# INLINE monosetInsertMany #-}
monosetInsertMany :: (Ord k, Foldable f) => f (TypeIndex k) -> MonomorphicSet (TypeIndex k) -> MonomorphicSet (TypeIndex k)
monosetInsertMany = flip (foldr monosetInsert)

{-# INLINE localMonoset #-}
localMonoset :: (MonomorphicSet (o k) -> MonomorphicSet (o k)) -> AggregationStack c o k t a -> AggregationStack c o k t a
localMonoset = local . overAggregationMonomorphicSet
