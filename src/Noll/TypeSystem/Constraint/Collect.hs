{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint.Collect (
  ConstraintsContext (..),
  Constraints (..),
  CollectConstraints,
  collectConstraints,
  runCollectConstraints,
  evalCollectConstraints,
)
where

import Control.Monad (forM)
import Control.Monad.RWS (MonadRWS, MonadReader, MonadState, MonadWriter, RWS, asks, evalRWS, local, tell)
import Data.List (partition)
import qualified Data.Set as Set
import Noll.Label (Label (..))
import Noll.Language.Expression (Expression (..))
import qualified Noll.Language.Expression as Expr
import Noll.Language.Expression.Binding (Binding)
import qualified Noll.Language.Expression.Binding as Binding
import Noll.Language.HasType (HasType (..))
import Noll.Language.HasTypeIndexes (HasTypeIndexes (..))
import Noll.Language.Pattern (Pattern (..))
import qualified Noll.Language.Pattern as Pattern
import Noll.Language.Type (Type (..), foldType)
import Noll.Language.Type.Index (TypeIndex (..))
import qualified Noll.Language.Type.Intrinsic as Intrinsic
import Noll.Language.Type.Opaque (OpaqueType)
import Noll.TypeSystem.Assumption (Assumption (..), assumptionNameIs)
import Noll.TypeSystem.Constraint (MonomorphicSet (..), TypeConstraint (..), overMonomorphicSet)
import Noll.Utils ((<$$>))

data ConstraintsContext o k = ConstraintsContext
  { contextMonomorphicSet :: MonomorphicSet (o k)
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE overContextMonomorphicSet #-}
overContextMonomorphicSet :: (MonomorphicSet (o k) -> MonomorphicSet (o k)) -> ConstraintsContext o k -> ConstraintsContext o k
overContextMonomorphicSet fn ConstraintsContext{..} = ConstraintsContext{contextMonomorphicSet = fn contextMonomorphicSet, ..}

type ConstraintsMonad o k t = RWS (ConstraintsContext o k) [TypeConstraint o k t] ()

newtype Constraints o k t a = Constraints {constraintsMonad :: ConstraintsMonad o k t a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (ConstraintsContext o k)
    , MonadWriter [TypeConstraint o k t]
    , MonadState ()
    , MonadRWS (ConstraintsContext o k) [TypeConstraint o k t] ()
    )

{-# INLINE monosetInsert #-}
monosetInsert :: (Ord k) => TypeIndex k -> MonomorphicSet (TypeIndex k) -> MonomorphicSet (TypeIndex k)
monosetInsert = overMonomorphicSet . Set.insert

{-# INLINE monosetInsertMany #-}
monosetInsertMany :: (Ord k, Foldable f) => f (TypeIndex k) -> MonomorphicSet (TypeIndex k) -> MonomorphicSet (TypeIndex k)
monosetInsertMany = flip (foldr monosetInsert)

{-# INLINE localMonoset #-}
localMonoset :: (MonomorphicSet (o k) -> MonomorphicSet (o k)) -> Constraints o k t a -> Constraints o k t a
localMonoset = local . overContextMonomorphicSet

type CollectConstraints = Constraints TypeIndex () OpaqueType

{-# INLINE runCollectConstraints #-}
runCollectConstraints :: ConstraintsContext o k -> Constraints o k t a -> (a, [TypeConstraint o k t])
runCollectConstraints cc cs = evalRWS (constraintsMonad cs) cc ()

{-# INLINE evalCollectConstraints #-}
evalCollectConstraints :: ConstraintsContext o k -> Constraints o k t a -> [TypeConstraint o k t]
evalCollectConstraints = snd <$$> runCollectConstraints

{-# INLINE assertEquality #-}
assertEquality :: (HasType TypeIndex () a, HasType TypeIndex () b) => a -> b -> CollectConstraints ()
assertEquality a1 a2 = tell [Equality (typeOf a1) (typeOf a2)]

assertEqualityAssumptions :: OpaqueType -> [Assumption OpaqueType] -> CollectConstraints ()
assertEqualityAssumptions t ms =
  tell $ do
    Assumption{..} <- ms
    pure (Equality assumptionType t)

assertImplicitAssumptions :: OpaqueType -> [Assumption OpaqueType] -> CollectConstraints ()
assertImplicitAssumptions t ms = do
  set <- asks contextMonomorphicSet
  tell $ do
    Assumption{..} <- ms
    pure (Implicit assumptionType t set)

patternAssumptions :: [Assumption OpaqueType] -> Pattern OpaqueType -> CollectConstraints [Assumption OpaqueType]
patternAssumptions ms =
  \case
    Pattern.Variable (Label t name) -> do
      let (ls, rs) = partition (assumptionNameIs name) ms
      assertEqualityAssumptions t ls
      pure rs

collectConstraints :: Expression OpaqueType -> CollectConstraints [Assumption OpaqueType]
collectConstraints =
  \case
    Expr.Constructor (Label _ name) -> do
      undefined
    Expr.Variable (Label t name) -> do
      pure [Assumption name t]
    Expr.Lambda ps e -> do
      ms1 <- localMonoset (monosetInsertMany (typeIndexesIn ps)) (collectConstraints e)
      ms2 <- concat <$> forM ps (patternAssumptions ms1)
      pure ms2
    Expr.Let gs e1 -> do
      ms1 <- collectConstraints e1
      ms2 <- forEachBinding gs $
        \case
          Binding.Pattern (Pattern.Variable (Label t name)) e -> do
            ms <- collectConstraints e
            assertEquality t e
            pure ms
      ms3 <- forEachBinding gs $
        \case
          Binding.Pattern (Pattern.Variable (Label t name)) e -> do
            let (ls, rs) = partition (assumptionNameIs name) ms1
            assertImplicitAssumptions t ls
            pure rs
      pure (ms1 <> ms2 <> ms3)
    Expr.If e1 e2 e3 -> do
      ms1 <- collectConstraints e1
      ms2 <- collectConstraints e2
      ms3 <- collectConstraints e3
      assertEquality e1 (Intrinsic Intrinsic.Bool :: OpaqueType)
      assertEquality e2 e3
      pure (ms1 <> ms2 <> ms3)
    Expr.Application t e1 es -> do
      ms1 <- collectConstraints e1
      ms2 <- concat <$> traverse collectConstraints es
      assertEquality e1 (foldType t (typeOf <$> es))
      pure (ms1 <> ms2)
    Expr.Literal{} ->
      pure []

forEachBinding ::
  (Traversable f) =>
  f (Binding Expression OpaqueType) ->
  (Binding Expression OpaqueType -> CollectConstraints [a]) ->
  CollectConstraints [a]
forEachBinding gs = concat <$$> forM gs
