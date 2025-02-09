{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.SystemF.Constraint.Generation.Internal (
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
  overConstraintsGenStateTypeIndexes,
  updateConstraintsGenSupply,
) where

import Control.Monad.RWS (
  MonadRWS,
  MonadReader,
  MonadState,
  MonadWriter,
  RWS,
  evalRWS,
  local,
  modify,
  runRWS,
 )
import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (transformBi)
import Noll.Common.Environment (Environment (..))
import Noll.Common.Supply (Supply (..))
import Noll.Language (Constructor (..), Kind (..), Type (..), TypeIndex (..))
import Noll.SystemF.Constraint (
  Constraint (..),
  MonomorphicSet (..),
  overMonomorphicSet,
 )
import Noll.SystemF.Constraint.Generation.InferenceRule (InferenceRule (..))
import Noll.SystemF.Substitution (Substitutable (..), applyT)
import Noll.Utils (Dictionary, Name)

import qualified Data.Set as Set

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
    NonDistinctParametereters [[(Name, a)]]
  | -- | Type parameter resolves to a concrete type; e.g., fn(x : a, y : int32) => x + y
    ResolvesToMonomorphicType Name (Type TypeIndex Kind)
  deriving (Show, Eq, Ord, Read)

data ConstraintsGenError a
  = NoDataConstructor a Name
  | DataConstructorArityMismatch a Name Int Int
  | IllFormedTypeAnnotation (TypeAnnotationError a)
  deriving (Show, Eq, Ord, Read)

data ConstraintsGenContext o k t = ConstraintsGenContext
  { constraintsGenContextMonomorphicSet :: MonomorphicSet (o k)
  , constraintsGenContextDataConstructorEnv :: Environment (Constructor o k t)
  , constraintsGenContextTypeConstructorEnv :: Environment k
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE overConstraintsGenMonomorphicSet #-}
overConstraintsGenMonomorphicSet :: (MonomorphicSet (o k) -> MonomorphicSet (o k)) -> ConstraintsGenContext o k t -> ConstraintsGenContext o k t
overConstraintsGenMonomorphicSet fn ConstraintsGenContext{..} = ConstraintsGenContext{constraintsGenContextMonomorphicSet = fn constraintsGenContextMonomorphicSet, ..}

data ConstraintsGenState c = ConstraintsGenState
  { constraintsGenStateTypeIndexes :: Dictionary (c, TypeIndex Kind)
  , constraintsGenStateSupply :: Int
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE overConstraintsGenStateTypeIndexes #-}
overConstraintsGenStateTypeIndexes :: (Dictionary (c, TypeIndex Kind) -> Dictionary (c, TypeIndex Kind)) -> ConstraintsGenState c -> ConstraintsGenState c
overConstraintsGenStateTypeIndexes fn ConstraintsGenState{..} = ConstraintsGenState{constraintsGenStateTypeIndexes = fn constraintsGenStateTypeIndexes, ..}

{-# INLINE overConstraintsGenStateSupply #-}
overConstraintsGenStateSupply :: (Int -> Int) -> ConstraintsGenState c -> ConstraintsGenState c
overConstraintsGenStateSupply fn ConstraintsGenState{..} = ConstraintsGenState{constraintsGenStateSupply = fn constraintsGenStateSupply, ..}

instance Supply (ConstraintsGenState c) where
  updateSupply = overConstraintsGenStateSupply
  getSupply = constraintsGenStateSupply

type ConstraintsGenOutput c o k t = Either (ConstraintsGenError c) (Constraint (InferenceRule k c) o k t)

type ConstraintsGenMonad c o k t = RWS (ConstraintsGenContext o k t) [ConstraintsGenOutput c o k t] (ConstraintsGenState c)

newtype ConstraintsGenStack c o k t a = ConstraintsGenStack {constraintsGenMonad :: ConstraintsGenMonad c o k t a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (ConstraintsGenContext o k t)
    , MonadWriter [ConstraintsGenOutput c o k t]
    , MonadState (ConstraintsGenState c)
    , MonadRWS (ConstraintsGenContext o k t) [ConstraintsGenOutput c o k t] (ConstraintsGenState c)
    )

{-# INLINE evalConstraintsGenStack #-}
evalConstraintsGenStack :: Int -> ConstraintsGenContext o k t -> ConstraintsGenStack c o k t a -> (a, [ConstraintsGenOutput c o k t])
evalConstraintsGenStack supply ctx a = evalRWS (constraintsGenMonad a) ctx (ConstraintsGenState mempty supply)

{-# INLINE runConstraintsGenStack #-}
runConstraintsGenStack :: Int -> ConstraintsGenContext o k t -> ConstraintsGenStack c o k t a -> (a, ConstraintsGenState c, [ConstraintsGenOutput c o k t])
runConstraintsGenStack supply ctx a = runRWS (constraintsGenMonad a) ctx (ConstraintsGenState mempty supply)

{-# INLINE updateConstraintsGenSupply #-}
updateConstraintsGenSupply :: Int -> ConstraintsGenStack c o k t ()
updateConstraintsGenSupply supply = modify (overConstraintsGenStateSupply (const supply))

-- {-# INLINE getConstraintsGenSupply #-}
-- getConstraintsGenSupply :: ConstraintsGenStack c o k t Int
-- getConstraintsGenSupply = gets constraintsGenStateSupply

{-# INLINE monosetInsert #-}
monosetInsert :: (Ord k) => TypeIndex k -> MonomorphicSet (TypeIndex k) -> MonomorphicSet (TypeIndex k)
monosetInsert = overMonomorphicSet . Set.insert

{-# INLINE monosetInsertMultiple #-}
monosetInsertMultiple :: (Ord k, Foldable f) => f (TypeIndex k) -> MonomorphicSet (TypeIndex k) -> MonomorphicSet (TypeIndex k)
monosetInsertMultiple = flip (foldr monosetInsert)

{-# INLINE localMonoset #-}
localMonoset :: (MonomorphicSet (o k) -> MonomorphicSet (o k)) -> ConstraintsGenStack c o k t a -> ConstraintsGenStack c o k t a
localMonoset = local . overConstraintsGenMonomorphicSet
