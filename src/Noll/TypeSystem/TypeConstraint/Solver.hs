{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.TypeConstraint.Solver (SolverConstraint, solveTypes) where

import Control.Monad.Except (runExceptT)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadWriter)
import Data.Foldable (foldrM)
import Data.List (delete, find)
import Data.Set (intersection, (\\))
import Noll.TypeSystem.Solver (SolverError (..))
import qualified Data.Set as Set
import Noll.Language (
  HasTypeIndexes (..),
  Kind (..),
  KindIndex (..),
  Scheme (..),
  Type (..),
  TypeIndex (..),
  activeIdsIn,
  notBoundIn,
  typeIdsIn,
 )
import Noll.Library.Supply (supply)
import Noll.TypeSystem.TypeConstraint (MonomorphicSet (..), TypeConstraint (..))
import Noll.TypeSystem.TypeSubstitution (
  TypeSubstitutable (..),
  TypeSubstitution (..),
  mapsToType,
 )
import Noll.TypeSystem.TypeUnification (TypeUnifiable (..))

type SolverConstraint c k t = TypeConstraint c TypeIndex k t

isSolvable ::
  ( Ord k
  , HasTypeIndexes k t
  ) =>
  [SolverConstraint c k t] ->
  SolverConstraint c k t ->
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
  [SolverConstraint c k t] ->
  SolverChoice (SolverConstraint c k t)
choice cs = findChoice [(delete c cs, c) | c <- cs]
 where
  findChoice ps =
    maybe NoneFound (uncurry Choice) (find (uncurry isSolvable) ps)

solveTypes ::
  ( MonadState Int m
  , MonadWriter [SolverError] m
  , Eq c
  ) =>
  [SolverConstraint c (Kind KindIndex) (Type TypeIndex (Kind KindIndex))] ->
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
