{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.ConstraintSolver (
  Solver (..),
  SolverError (..),
  runSolver,
  solveTypes,
  solveKinds,
) where

import Control.Monad.Except (runExceptT)
import Control.Monad.RWS (MonadState, MonadWriter, RWS, runRWS, tell)
import Data.List (delete, find)
import Data.Set (intersection, (\\))
import qualified Data.Set as Set
import Noll.Language (Kind (..), KindIndex (..), Scheme (..), Type (..), TypeIndex (..), TypeIndexed (..), activeIdsIn, notBoundIn, typeIdsIn)
import Noll.Library.Supply (supply)
import Noll.TypeSystem.KindConstraint (KindConstraint (..))
import Noll.TypeSystem.KindSubstitution (KindSubstitutable (..), KindSubstitution)
import qualified Noll.TypeSystem.KindSubstitution as KindSubstitution
import Noll.TypeSystem.KindUnification (KindUnifiable (..))
import qualified Noll.TypeSystem.KindUnification as KindUnification
import Noll.TypeSystem.TypeConstraint (MonomorphicSet (..), TypeConstraint (..))
import Noll.TypeSystem.TypeSubstitution (TypeSubstitutable (..), TypeSubstitution (..), mapsToType)
import qualified Noll.TypeSystem.TypeSubstitution as TypeSubstitution
import qualified Noll.TypeSystem.TypeUnification as TypeUnification
import Noll.Utils (foldrM)

data SolverError c = SolverError
  { errorContext :: c
  }
  deriving (Show, Eq, Ord, Read)

instance (TypeSubstitutable c) => TypeSubstitutable (SolverError c) where
  apply sub SolverError{..} =
    SolverError{errorContext = TypeSubstitution.apply sub errorContext, ..}

instance (KindSubstitutable c) => KindSubstitutable (SolverError c) where
  apply sub SolverError{..} =
    SolverError{errorContext = KindSubstitution.apply sub errorContext, ..}

newtype Solver c a = Solver {solverMonad :: RWS () [SolverError c] (TypeIndex ()) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState (TypeIndex ())
    , MonadWriter [SolverError c]
    )

{-# INLINE runSolver #-}
runSolver :: Int -> Solver c a -> (a, [SolverError c])
runSolver n u = let (a, _, w) = runRWS (solverMonad u) () (TypeIndex () n) in (a, w)

isSolvable ::
  ( Ord k
  , TypeIndexed k t
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
  , TypeIndexed k t
  ) =>
  [TypeConstraint c TypeIndex k t] ->
  SolverChoice (TypeConstraint c TypeIndex k t)
choice cs = findChoice [(delete c cs, c) | c <- cs]
 where
  findChoice ps =
    maybe NoneFound (uncurry Choice) (find (uncurry isSolvable) ps)

solveTypes ::
  ( MonadState (TypeIndex ()) m
  , MonadWriter [SolverError c] m
  , Eq c
  ) =>
  [TypeConstraint c TypeIndex () (Type TypeIndex ())] ->
  m TypeSubstitution
solveTypes [] = pure (TypeSubstitution mempty)
solveTypes constraints =
  case choice constraints of
    NoneFound ->
      pure mempty
    Choice cs (Equality meta ts) -> do
      res <- runExceptT (TypeUnification.unifyAll ts)
      case res of
        Left err -> do
          tell [SolverError meta]
          solveTypes cs
        Right sub1 -> do
          sub2 <- solveTypes (TypeSubstitution.apply sub1 cs)
          pure (sub2 <> sub1)
    Choice cs (Implicit meta t1 t2 m) -> do
      solveTypes (Explicit meta t1 (generalize m t2) : cs)
    Choice cs (Explicit meta t1 s) -> do
      t2 <- instantiate s
      solveTypes (Equality meta [t1, t2] : cs)

instantiate ::
  (MonadState (TypeIndex ()) m) =>
  Scheme TypeIndex () (Type TypeIndex ()) ->
  m (Type TypeIndex ())
instantiate (Forall qs _ t) = do
  sub <- foldrM go mempty qs
  pure (TypeSubstitution.apply sub t)
 where
  go (TypeIndex _ index) sub = do
    s <- supply
    pure (index `mapsToType` TVariable s <> sub)

generalize ::
  (TypeIndexed k t) =>
  MonomorphicSet (TypeIndex k) ->
  t ->
  Scheme TypeIndex k t
generalize (MonomorphicSet m) t = Forall (notBoundIn m (typeIndexesIn t)) [] t

solveKinds ::
  ( KindSubstitutable k
  , KindUnifiable k
  , MonadWriter [SolverError c] m
  ) =>
  [KindConstraint c k] ->
  m KindSubstitution
solveKinds [] =
  pure mempty
solveKinds (KindEquality meta k1 k2 : cs) = do
  res <- runExceptT (KindUnification.unify k1 k2)
  case res of
    Left err -> do
      tell [SolverError meta]
      solveKinds cs
    Right sub1 -> do
      sub2 <- solveKinds (KindSubstitution.apply sub1 cs)
      pure (sub2 <> sub1)
