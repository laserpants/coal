{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint.Solver (
  Solver (..),
  runSolver,
  solve,
  solveConstraints,
) where

import Control.Monad.RWS (MonadState, MonadWriter, RWS, get, put, runRWS, tell)
import Data.Data (Data)
import Data.List (delete, find)
import Data.Set (intersection, (\\))
import Lang.Common.Supply (supply)
import Extra (foldrM)
import Noll.Language (
  IndexedType,
  IndexedScheme,
  Kind (..),
  Scheme (..),
  Type (..),
  TypeIndex (..),
  TypeIndexed (..),
  activeIdsIn,
  notBoundIn,
  typeIdsIn,
 )
import Noll.TypeSystem.Constraint (Constraint (..), Monomorphic (..))
import Noll.TypeSystem.Substitution (Substitutable (..), Substitution (..), mapsTo)
import Noll.TypeSystem.Unification (UnificationError, Unifier (..), runUnifier, unifyAll)

import qualified Data.Set as Set

transUnifier :: Unifier a -> Solver s (Either UnificationError a)
transUnifier u = do
  (r, q) <- runUnifier <$> get <*> pure u
  put q
  return r

newtype Solver s t = Solver {solverMonad :: RWS () [s] Int t}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState Int
    , MonadWriter [s]
    )

type SolverConstraint s = Constraint s TypeIndex Kind IndexedType

{-# INLINE solveConstraints #-}
solveConstraints :: (Eq s, Data s) => Int -> [SolverConstraint s] -> (Substitution, Int, [s])
solveConstraints n cs = runSolver n (solve cs)

{-# INLINE runSolver #-}
runSolver :: Int -> Solver s t -> (t, Int, [s])
runSolver n s = runRWS (solverMonad s) () n

isSolvable :: [SolverConstraint s] -> SolverConstraint s -> Bool
isSolvable constraints =
  \case
    Implicit _ _ t2 m ->
      Set.null (typeIdsIn t2 \\ typeIdsIn m `intersection` activeIdsIn constraints)
    _ ->
      True

data SolverChoice c = Choice [c] c | ChoiceNotFound
  deriving (Show, Eq, Ord, Read)

choice :: (Eq s) => [SolverConstraint s] -> SolverChoice (SolverConstraint s)
choice cs = findChoice [(delete c cs, c) | c <- cs]
 where
  findChoice = maybe ChoiceNotFound (uncurry Choice) . find (uncurry isSolvable)

solve :: (Eq s, Data s) => [SolverConstraint s] -> Solver s Substitution
solve [] = pure (Substitution mempty)
solve constraints =
  case choice constraints of
    ChoiceNotFound ->
      pure mempty
    Choice cs (Equality c ts) -> do
      res <- transUnifier (unifyAll ts)
      case res of
        Left{} -> do
          tell [c]
          solve cs
        Right sub1 -> do
          sub2 <- solve (apply sub1 cs)
          pure (sub2 <> sub1)
    Choice cs (Implicit c t1 t2 m) ->
      solve (Explicit c t1 (generalize m t2) : cs)
    Choice cs (Explicit c t1 s) -> do
      t2 <- instantiate s
      solve (Equality c [t1, t2] : cs)

{-# INLINE generalize #-}
generalize :: (TypeIndexed k t) => Monomorphic (TypeIndex k) -> t -> Scheme TypeIndex k t
generalize (Monomorphic m) t = Forall (notBoundIn m (typeIndexesIn t)) [] t

instantiate :: IndexedScheme -> Solver s IndexedType
instantiate (Forall qs _ t) = do
  sub <- foldrM go mempty qs
  pure (apply sub t)
 where
  go (TypeIndex k index) sub = do
    s <- supply
    pure (index `mapsTo` TVariable (TypeIndex k s) <> sub)
