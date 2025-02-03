{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.SystemF.Constraint.Solver (
  Solver (..),
  runSolver,
  solve,
  solveConstraints,
) where

import Control.Monad.RWS (MonadState, MonadWriter, RWS, get, put, runRWS, tell)
import Data.List (delete, find)
import Data.Set (intersection, (\\))
import Noll.Common.Supply (supply)
import Noll.Language (
  IndexedType,
  Kind (..),
  Scheme (..),
  Type (..),
  TypeIndex (..),
  TypeIndexed (..),
  activeIdsIn,
  notBoundIn,
  typeIdsIn,
 )
import Noll.SystemF.Constraint (Constraint (..), MonomorphicSet (..))
import Noll.SystemF.Substitution (Substitutable (..), Substitution (..), mapsTo)
import Noll.SystemF.Unification (UnificationError, Unifier (..), runUnifier, unifyAll)
import Noll.Utils (foldrM)

import qualified Data.Set as Set

liftUnifier :: Unifier a -> Solver c (Either UnificationError a)
liftUnifier u = do
  (r, q) <- runUnifier <$> get <*> pure u
  put q
  pure r

newtype Solver c t = Solver {solverMonad :: RWS () [c] Int t}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState Int
    , MonadWriter [c]
    )

{-# INLINE solveConstraints #-}
solveConstraints :: (Show c, Eq c) => Int -> [Constraint c TypeIndex Kind IndexedType] -> (Substitution, Int, [c])
solveConstraints sup cs = runSolver sup (solve cs)

{-# INLINE runSolver #-}
runSolver :: Int -> Solver c t -> (t, Int, [c])
runSolver sup s = runRWS (solverMonad s) () sup

isSolvable :: (Ord k, TypeIndexed k t) => [Constraint c TypeIndex k t] -> Constraint c TypeIndex k t -> Bool
isSolvable constraints =
  \case
    Implicit _ _ t2 m ->
      Set.null (typeIdsIn t2 \\ typeIdsIn m `intersection` activeIdsIn constraints)
    _ ->
      True

data SolverChoice c = Choice [c] c | ChoiceNotFound
  deriving (Show, Eq, Ord, Read)

choice :: (Ord k, Eq t, Eq c, TypeIndexed k t) => [Constraint c TypeIndex k t] -> SolverChoice (Constraint c TypeIndex k t)
choice cs = findChoice [(delete c cs, c) | c <- cs]
 where
  findChoice ps =
    maybe ChoiceNotFound (uncurry Choice) (find (uncurry isSolvable) ps)

solve :: (Show c, Eq c) => [Constraint c TypeIndex Kind IndexedType] -> Solver c Substitution
solve [] = pure (Substitution mempty)
solve constraints =
  case choice constraints of
    ChoiceNotFound ->
      pure mempty
    Choice cs (Equality c ts) -> do
      res <- liftUnifier (unifyAll ts)
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
generalize :: (TypeIndexed k t) => MonomorphicSet (TypeIndex k) -> t -> Scheme TypeIndex k t
generalize (MonomorphicSet m) t = Forall (notBoundIn m (typeIndexesIn t)) [] t

instantiate :: Scheme TypeIndex Kind IndexedType -> Solver c IndexedType
instantiate (Forall qs _ t) = do
  sub <- foldrM go mempty qs
  pure (apply sub t)
 where
  go (TypeIndex k index) sub = do
    s <- supply
    pure (index `mapsTo` TVariable (TypeIndex k s) <> sub)
