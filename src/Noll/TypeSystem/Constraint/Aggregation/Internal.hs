{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint.Aggregation.Internal (
  TypeAnnotationError,
  ConstraintsGenerationError (..),
  TypeAnnotationError (..),
  AggregationContext (..),
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
import Noll.TypeSystem.Constraint.Rule (InferenceRule (..))
import Noll.Utils (Dictionary, Name)

import qualified Data.Set as Set

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

data AggregationContext o k t = AggregationContext
  { aggregationMonomorphicSet :: MonomorphicSet (o k)
  , aggregationDataConstructorEnv :: Environment (Constructor o k t)
  , aggregationTypeConstructorEnv :: Environment k
  , aggregationIndexTreshold :: Int
  }
  deriving (Show, Eq, Ord, Read)

type AggregationOutput c o k t = Either (ConstraintsGenerationError c) (Constraint (InferenceRule k c) o k t)

type AggregationMonad c o k t = RWS (AggregationContext o k t) [AggregationOutput c o k t] (Dictionary (c, TypeIndex Kind))

newtype AggregationStack c o k t a = AggregationStack {aggregationMonad :: AggregationMonad c o k t a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (AggregationContext o k t)
    , MonadWriter [AggregationOutput c o k t]
    , MonadState (Dictionary (c, TypeIndex Kind))
    , MonadRWS (AggregationContext o k t) [AggregationOutput c o k t] (Dictionary (c, TypeIndex Kind))
    )

{-# INLINE evalAggregationStack #-}
evalAggregationStack :: AggregationContext o k t -> AggregationStack c o k t a -> (a, [AggregationOutput c o k t])
evalAggregationStack ctx a = evalRWS (aggregationMonad a) ctx mempty

{-# INLINE runAggregationStack #-}
runAggregationStack :: AggregationContext o k t -> AggregationStack c o k t a -> (a, Dictionary (c, TypeIndex Kind), [AggregationOutput c o k t])
runAggregationStack ctx a = runRWS (aggregationMonad a) ctx mempty

{-# INLINE overAggregationMonomorphicSet #-}
overAggregationMonomorphicSet :: (MonomorphicSet (o k) -> MonomorphicSet (o k)) -> AggregationContext o k t -> AggregationContext o k t
overAggregationMonomorphicSet fn AggregationContext{..} = AggregationContext{aggregationMonomorphicSet = fn aggregationMonomorphicSet, ..}

{-# INLINE monosetInsert #-}
monosetInsert :: (Ord k) => TypeIndex k -> MonomorphicSet (TypeIndex k) -> MonomorphicSet (TypeIndex k)
monosetInsert = overMonomorphicSet . Set.insert

{-# INLINE monosetInsertMany #-}
monosetInsertMany :: (Ord k, Foldable f) => f (TypeIndex k) -> MonomorphicSet (TypeIndex k) -> MonomorphicSet (TypeIndex k)
monosetInsertMany = flip (foldr monosetInsert)

{-# INLINE localMonoset #-}
localMonoset :: (MonomorphicSet (o k) -> MonomorphicSet (o k)) -> AggregationStack c o k t a -> AggregationStack c o k t a
localMonoset = local . overAggregationMonomorphicSet
