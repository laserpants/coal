{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint.Aggregation.Internal (
  TypeAnnotationError,
  AggregationError (..),
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
import qualified Data.Set as Set
import Noll.Language (Constructor (..), Kind (..), Type (..), TypeIndex (..))
import Noll.Library.Environment (Environment (..))
import Noll.TypeSystem.Constraint (Constraint (..), MonomorphicSet (..), overMonomorphicSet)
import Noll.TypeSystem.Constraint.Rule (InferenceRule (..))
import Noll.Utils (Dictionary, Name)

data TypeAnnotationError a
  = KindMismatch a
  | NoTypeConstructor a Name
  | NonDistinctTypeParameters [[(Name, a)]]
  | ResolvesToConcreteType Name (Type TypeIndex Kind)
  deriving (Show, Eq, Ord, Read)

data AggregationError a
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

type AggregationOutput w o k t = Either (AggregationError w) (Constraint (InferenceRule k w) o k t)

type AggregationMonad w o k t = RWS (AggregationContext o k t) [AggregationOutput w o k t] (Dictionary (w, TypeIndex Kind))

newtype AggregationStack w o k t a = AggregationStack {aggregationMonad :: AggregationMonad w o k t a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (AggregationContext o k t)
    , MonadWriter [AggregationOutput w o k t]
    , MonadState (Dictionary (w, TypeIndex Kind))
    , MonadRWS (AggregationContext o k t) [AggregationOutput w o k t] (Dictionary (w, TypeIndex Kind))
    )

{-# INLINE evalAggregationStack #-}
evalAggregationStack :: AggregationContext o k t -> AggregationStack w o k t a -> (a, [AggregationOutput w o k t])
evalAggregationStack ctx m = evalRWS (aggregationMonad m) ctx mempty

{-# INLINE runAggregationStack #-}
runAggregationStack :: AggregationContext o k t -> AggregationStack w o k t a -> (a, Dictionary (w, TypeIndex Kind), [AggregationOutput w o k t])
runAggregationStack ctx m = runRWS (aggregationMonad m) ctx mempty

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
localMonoset :: (MonomorphicSet (o k) -> MonomorphicSet (o k)) -> AggregationStack w o k t a -> AggregationStack w o k t a
localMonoset = local . overAggregationMonomorphicSet
