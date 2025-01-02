{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint.Solver (runSolver, solve, solveConstraints) where

import Control.Monad.Except (runExceptT)
import Control.Monad.RWS (MonadState, MonadWriter, RWS, runRWS, tell)
import Data.List (delete, find)
import Data.Set (intersection, (\\))
import qualified Data.Set as Set
import Noll.Language (Kind (..), KindIndex, Scheme (..), Type (..), TypeIndex (..), TypeIndexed (..), activeIdsIn, freshIdIn, notBoundIn, typeIdsIn)
import Noll.Library.Supply (supply)
import Noll.TypeSystem.Constraint (Constraint (..), MonomorphicSet (..))
import Noll.TypeSystem.Substitution (Substitutable (..), Substitution (..), mapsTo)
import Noll.TypeSystem.Unification (unifyAll)
import Noll.Utils (foldrM)

newtype Solver c a = Solver {solverMonad :: RWS () [c] (TypeIndex (Kind KindIndex)) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState (TypeIndex (Kind KindIndex))
    , MonadWriter [c]
    )

{-# INLINE solveConstraints #-}
solveConstraints :: (Eq c) => [Constraint c TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))] -> (Substitution, [c])
solveConstraints cs = runSolver (freshIdIn cs) (solve cs)

{-# INLINE runSolver #-}
runSolver :: Int -> Solver c a -> (a, [c])
runSolver n u = (a, w)
 where
  (a, _, w) = runRWS (solverMonad u) () (TypeIndex KType n)

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

solve :: (Eq c) => [Constraint c TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))] -> Solver c Substitution
solve [] = pure (Substitution mempty)
solve constraints =
  case choice constraints of
    ChoiceNotFound ->
      pure mempty
    Choice cs (Equality c ts) -> do
      res <- runExceptT (unifyAll ts)
      case res of
        Left err -> do
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

instantiate :: Scheme TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex)) -> Solver c (Type TypeIndex (Kind KindIndex))
instantiate (Forall qs _ t) = do
  sub <- foldrM go mempty qs
  pure (apply sub t)
 where
  go (TypeIndex _ index) sub = do
    s <- supply
    pure (index `mapsTo` TVariable s <> sub)
