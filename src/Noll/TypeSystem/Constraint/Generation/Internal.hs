{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint.Generation.Internal (
  TypeAnnotationError,
  ConstraintsGenerationError (..),
  InferenceRule (..),
  TypeAnnotationError (..),
  ConstraintsGenerationContext (..),
  ConstraintsGenerationStack (..),
  ConstraintsGenerationOutput (..),
  monosetInsert,
  monosetInsertMany,
  localMonoset,
  runConstraintsGenerationStack,
  evalConstraintsGenerationStack,
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
import Noll.Lib.Environment (Environment (..))
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
  { constraintsGenerationMonomorphicSet :: MonomorphicSet (o k)
  , constraintsGenerationDataConstructorEnv :: Environment (Constructor o k t)
  , constraintsGenerationTypeConstructorEnv :: Environment k
  , constraintsGenerationIndexTreshold :: Int
  }
  deriving (Show, Eq, Ord, Read)

type ConstraintsGenerationOutput c o k t = Either (ConstraintsGenerationError c) (Constraint (InferenceRule k c) o k t)

type ConstraintsGenerationMonad c o k t = RWS (ConstraintsGenerationContext o k t) [ConstraintsGenerationOutput c o k t] (Dictionary (c, TypeIndex Kind))

newtype ConstraintsGenerationStack c o k t a = ConstraintsGenerationStack {constraintsGenerationMonad :: ConstraintsGenerationMonad c o k t a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (ConstraintsGenerationContext o k t)
    , MonadWriter [ConstraintsGenerationOutput c o k t]
    , MonadState (Dictionary (c, TypeIndex Kind))
    , MonadRWS (ConstraintsGenerationContext o k t) [ConstraintsGenerationOutput c o k t] (Dictionary (c, TypeIndex Kind))
    )

{-# INLINE evalConstraintsGenerationStack #-}
evalConstraintsGenerationStack :: ConstraintsGenerationContext o k t -> ConstraintsGenerationStack c o k t a -> (a, [ConstraintsGenerationOutput c o k t])
evalConstraintsGenerationStack ctx a = evalRWS (constraintsGenerationMonad a) ctx mempty

{-# INLINE runConstraintsGenerationStack #-}
runConstraintsGenerationStack :: ConstraintsGenerationContext o k t -> ConstraintsGenerationStack c o k t a -> (a, Dictionary (c, TypeIndex Kind), [ConstraintsGenerationOutput c o k t])
runConstraintsGenerationStack ctx a = runRWS (constraintsGenerationMonad a) ctx mempty

{-# INLINE overConstraintsGenerationMonomorphicSet #-}
overConstraintsGenerationMonomorphicSet :: (MonomorphicSet (o k) -> MonomorphicSet (o k)) -> ConstraintsGenerationContext o k t -> ConstraintsGenerationContext o k t
overConstraintsGenerationMonomorphicSet fn ConstraintsGenerationContext{..} = ConstraintsGenerationContext{constraintsGenerationMonomorphicSet = fn constraintsGenerationMonomorphicSet, ..}

{-# INLINE monosetInsert #-}
monosetInsert :: (Ord k) => TypeIndex k -> MonomorphicSet (TypeIndex k) -> MonomorphicSet (TypeIndex k)
monosetInsert = overMonomorphicSet . Set.insert

{-# INLINE monosetInsertMany #-}
monosetInsertMany :: (Ord k, Foldable f) => f (TypeIndex k) -> MonomorphicSet (TypeIndex k) -> MonomorphicSet (TypeIndex k)
monosetInsertMany = flip (foldr monosetInsert)

{-# INLINE localMonoset #-}
localMonoset :: (MonomorphicSet (o k) -> MonomorphicSet (o k)) -> ConstraintsGenerationStack c o k t a -> ConstraintsGenerationStack c o k t a
localMonoset = local . overConstraintsGenerationMonomorphicSet
