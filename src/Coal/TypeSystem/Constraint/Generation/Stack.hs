{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Coal.TypeSystem.Constraint.Generation.Stack (
  ConstraintsGenError (..),
  InferenceRule (..),
  TypeAnnotationError (..),
  ConstraintsGenContext (..),
  ConstraintsGenStack (..),
  ConstraintsGenOutput,
  ConstraintsGenState (..),
  monosetInsert,
  monosetInsertMultiple,
  localMonoset,
  runConstraintsGenStack,
  evalConstraintsGenStack,
  updateConstraintsGenSupply,
  emptyConstraintsGenContext,
) where

import Coal.Language (TypeIndex (..))
import Coal.TypeSystem.Constraint (Constraint (..), Monomorphic (..), overMonomorphicSet)
import Coal.TypeSystem.Constraint.Generation.Annotation.Error (TypeAnnotationError (..))
import Coal.TypeSystem.Constraint.Generation.Context (ConstraintsGenContext (..), emptyConstraintsGenContext, overConstraintsGenMonomorphicSet)
import Coal.TypeSystem.Constraint.Generation.Error (ConstraintsGenError (..))
import Coal.TypeSystem.Constraint.Generation.InferenceRule (InferenceRule (..))
import Coal.TypeSystem.Constraint.Generation.State (ConstraintsGenState (..), overConstraintsGenStateSupply)
import Control.Monad.RWS.Strict
import qualified Data.Set as Set

type ConstraintsGenOutput g o a t =
  Either
    (ConstraintsGenError g)
    (Constraint (InferenceRule a g) o a t)

type ConstraintsGenMonad g o a t =
  RWS
    (ConstraintsGenContext g o a t)
    [ConstraintsGenOutput g o a t]
    (ConstraintsGenState g)

newtype ConstraintsGenStack g o a t s = ConstraintsGenStack
  { constraintsGenMonad :: ConstraintsGenMonad g o a t s
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (ConstraintsGenContext g o a t)
    , MonadWriter [ConstraintsGenOutput g o a t]
    , MonadState (ConstraintsGenState g)
    , MonadRWS (ConstraintsGenContext g o a t) [ConstraintsGenOutput g o a t] (ConstraintsGenState g)
    )

evalConstraintsGenStack :: Int -> ConstraintsGenContext g o a t -> ConstraintsGenStack g o a t s -> (s, [ConstraintsGenOutput g o a t])
evalConstraintsGenStack supply ctx a = evalRWS (constraintsGenMonad a) ctx (ConstraintsGenState mempty supply)

runConstraintsGenStack :: Int -> ConstraintsGenContext g o a t -> ConstraintsGenStack g o a t s -> (s, ConstraintsGenState g, [ConstraintsGenOutput g o a t])
runConstraintsGenStack supply ctx a = runRWS (constraintsGenMonad a) ctx (ConstraintsGenState mempty supply)

{-# INLINE updateConstraintsGenSupply #-}
updateConstraintsGenSupply :: Int -> ConstraintsGenStack g o a t ()
updateConstraintsGenSupply supply = modify (overConstraintsGenStateSupply (const supply))

{-# INLINE monosetInsert #-}
monosetInsert :: (Ord k) => TypeIndex k -> Monomorphic (TypeIndex k) -> Monomorphic (TypeIndex k)
monosetInsert = overMonomorphicSet . Set.insert

{-# INLINE monosetInsertMultiple #-}
monosetInsertMultiple :: (Ord k, Foldable f) => f (TypeIndex k) -> Monomorphic (TypeIndex k) -> Monomorphic (TypeIndex k)
monosetInsertMultiple = flip (foldr monosetInsert)

{-# INLINE localMonoset #-}
localMonoset :: (Monomorphic (o a) -> Monomorphic (o a)) -> ConstraintsGenStack g o a t s -> ConstraintsGenStack g o a t s
localMonoset = local . overConstraintsGenMonomorphicSet
