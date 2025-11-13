{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.TypeSystem.Constraint.Generation.Internal (
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

import Coal.Common.Environment (Environment (..))
import Coal.Common.Supply (Supply (..))
import Coal.Compiler.Module.Bundle (ModuleBundle)
import Coal.Language (CodataAccessor (..), IndexedScheme, Kind (..), Type (..), TypeIndex (..))
import Coal.TypeSystem.Constraint (Constraint (..), Monomorphic (..), overMonomorphicSet)
import Coal.TypeSystem.Constraint.Generation.InferenceRule (InferenceRule (..))
import Control.Monad.RWS
import qualified Data.Set as Set
import Extras (Dictionary, Name)

data TypeAnnotationError a
  = -- Kind mismatch
    EAnnotationKindMismatch a
  | -- | Type constructor is not in scope
    EAnnotationConstructor a Name
  | -- | Two or more named parameters refer to the same inferred type variable.
    -- E.g., the annotation reads something like (a -> b) -> c -> b, but the
    -- function is fn(f, x) => f(x), which would require 'a' and 'c' to be the
    -- same type. The type signature claims that the function is polymorphic
    -- with respect to any choice of variables a, b, and c.
    EAnnotationNonDistinctParameter a Name
  | -- | Type parameter resolves to a concrete type; e.g.,
    -- fn(x : a, y : int32) => x + y
    EAnnotationMonomorphicType a Name (Type TypeIndex Kind)
  deriving (Show, Eq, Ord, Read)

data ConstraintsGenError a
  = ENoDataConstructor a Name
  | ENoCodataAccessor a Name
  | ECodataFieldMismatch a
  | EDataConstructorArityMismatch a Name Int Int
  | EIllFormedTypeAnnotation (TypeAnnotationError a)
  | EFoldPatternInRegularMatch a
  deriving (Show, Eq, Ord, Read)

data ConstraintsGenContext g o a t = ConstraintsGenContext
  { constraintsGenContextMonomorphicSet :: Monomorphic (o a)
  , constraintsGenContextModules :: ModuleBundle g
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE overConstraintsGenMonomorphicSet #-}
overConstraintsGenMonomorphicSet :: (Monomorphic (o a) -> Monomorphic (o a)) -> ConstraintsGenContext g o a t -> ConstraintsGenContext g o a t
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

type ConstraintsGenOutput g o a t = Either (ConstraintsGenError g) (Constraint (InferenceRule a g) o a t)

type ConstraintsGenMonad g o a t = RWS (ConstraintsGenContext g o a t) [ConstraintsGenOutput g o a t] (ConstraintsGenState g)

newtype ConstraintsGenStack g o a t s = ConstraintsGenStack {constraintsGenMonad :: ConstraintsGenMonad g o a t s}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (ConstraintsGenContext g o a t)
    , MonadWriter [ConstraintsGenOutput g o a t]
    , MonadState (ConstraintsGenState g)
    , MonadRWS (ConstraintsGenContext g o a t) [ConstraintsGenOutput g o a t] (ConstraintsGenState g)
    )

{-# INLINE evalConstraintsGenStack #-}
evalConstraintsGenStack :: Int -> ConstraintsGenContext g o a t -> ConstraintsGenStack g o a t s -> (s, [ConstraintsGenOutput g o a t])
evalConstraintsGenStack supply ctx a = evalRWS (constraintsGenMonad a) ctx (ConstraintsGenState mempty supply)

{-# INLINE runConstraintsGenStack #-}
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
