{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Solver (
  Solver (..),
  SolverError (..),
  runSolver,
  evalSolver,
  solveTypes,
  solveKinds,
) where

import Control.Monad.Except (runExceptT)
import Control.Monad.State (MonadState, State, runState)
import Control.Monad.Writer (MonadWriter, WriterT, runWriterT)
import Data.Foldable (foldrM)
import Data.List (delete, find)
import Data.Set (intersection, (\\))
import qualified Data.Set as Set
import Noll.Language (HasTypeIndexes (..), Kind (..), KindIndex (..), Scheme (..), Type (..), TypeIndex (..), activeIdsIn, notBoundIn, typeIdsIn)
import Noll.Library.Supply (supply)
import Noll.TypeSystem.KindConstraint (KindConstraint (..))
import Noll.TypeSystem.KindSubstitution (KindSubstitutable (..), KindSubstitution)
import Noll.TypeSystem.KindUnification (KindUnifiable (..))
import Noll.TypeSystem.TypeConstraint (MonomorphicSet (..), TypeConstraint (..))
import Noll.TypeSystem.TypeSubstitution (TypeSubstitutable (..), TypeSubstitution (..), mapsToType)
import Noll.TypeSystem.TypeUnification (TypeUnifiable (..))

data SolverError = SolverError
  deriving (Show, Eq, Ord, Read)

newtype Solver a = Solver {solverMonad :: WriterT [SolverError] (State Int) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState Int
    , MonadWriter [SolverError]
    )

{-# INLINE runSolver #-}
runSolver :: Int -> Solver a -> ((a, [SolverError]), Int)
runSolver n u = runState (runWriterT (solverMonad u)) n

{-# INLINE evalSolver #-}
evalSolver :: Int -> Solver a -> (a, [SolverError])
evalSolver n u = fst (runSolver n u)

isSolvable ::
  ( Ord k
  , HasTypeIndexes k t
  ) =>
  [TypeConstraint c TypeIndex k t] ->
  TypeConstraint c TypeIndex k t ->
  Bool
isSolvable constraints =
  \case
    Implicit _ _ t2 m ->
      Set.null (typeIdsIn t2 \\ typeIdsIn m `intersection` activeIdsIn constraints)
    _ ->
      True

data SolverChoice c
  = Choice [c] c
  | NoneFound
  deriving (Show, Eq, Ord, Read)

choice ::
  ( Ord k
  , Eq t
  , Eq c
  , HasTypeIndexes k t
  ) =>
  [TypeConstraint c TypeIndex k t] ->
  SolverChoice (TypeConstraint c TypeIndex k t)
choice cs = findChoice [(delete c cs, c) | c <- cs]
 where
  findChoice ps =
    maybe NoneFound (uncurry Choice) (find (uncurry isSolvable) ps)

solveTypes ::
  ( MonadState Int m
  , MonadWriter [SolverError] m
  , Eq c
  ) =>
  [TypeConstraint c TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))] ->
  m TypeSubstitution
solveTypes [] = pure (TypeSubstitution mempty)
solveTypes constraints =
  case choice constraints of
    NoneFound ->
      pure mempty
    Choice cs (Equality _ t1 t2) -> do
      res <- runExceptT (unify t1 t2)
      case res of
        Left err ->
          -- error "TODO"
          solveTypes cs
        Right sub1 -> do
          sub2 <- solveTypes (apply sub1 cs)
          pure (sub2 <> sub1)
    Choice cs (Implicit x t1 t2 m) -> do
      solveTypes (Explicit x t1 (generalize m t2) : cs)
    Choice cs (Explicit x t1 s) -> do
      t2 <- instantiate s
      solveTypes (Equality x t1 t2 : cs)

instantiate ::
  (MonadState Int m) =>
  Scheme TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex)) ->
  m (Type TypeIndex (Kind KindIndex))
instantiate (Forall qs _ t) = do
  sub <- foldrM go mempty qs
  pure (apply sub t)
 where
  go (TypeIndex k index) sub = do
    s <- supply
    pure (index `mapsToType` TVariable (TypeIndex k s) <> sub)

generalize ::
  (HasTypeIndexes k t) =>
  MonomorphicSet (TypeIndex k) ->
  t ->
  Scheme TypeIndex k t
generalize (MonomorphicSet m) t = Forall (notBoundIn m (typeIndexesIn t)) [] t

solveKinds ::
  ( KindSubstitutable k
  , KindUnifiable k
  , MonadWriter [SolverError] m
  ) =>
  [KindConstraint c k] ->
  m KindSubstitution
solveKinds [] =
  pure mempty
solveKinds (KindEquality _ k1 k2 : cs) = do
  res <- runExceptT (unifyKinds k1 k2)
  case res of
    Left err ->
      -- error "TODO"
      solveKinds cs
    Right sub1 -> do
      sub2 <- solveKinds (applyKindSub sub1 cs)
      pure (sub2 <> sub1)
