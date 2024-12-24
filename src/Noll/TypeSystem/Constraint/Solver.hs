{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint.Solver where

import Control.Monad.State (MonadState, get)
import Data.Foldable (foldrM)
import Data.List (delete, find)
import Data.Set (intersection, (\\))
import qualified Data.Set as Set
import Noll.Language.HasActive (activeIdsIn)
import Noll.Language.HasTypeIndexes (HasTypeIndexes (..), notBoundIn, typeIdsIn)
import Noll.Language.Type (Type (..))
import qualified Noll.Language.Type as Type
import Noll.Language.Type.Index (TypeIndex (..))
import Noll.Language.Type.Scheme (Scheme (..))
import Noll.TypeSystem.Constraint (MonomorphicSet (..), TypeConstraint (..))
import Noll.TypeSystem.Substitution (TypeSubstitutable (..), TypeSubstitution (..), mapsTo)
import Noll.TypeSystem.Unification (TypeUnifiable (..))

type SolverConstraint k t = TypeConstraint TypeIndex k t

isSolvable ::
  ( Ord k
  , HasTypeIndexes k t
  ) =>
  [SolverConstraint k t] ->
  SolverConstraint k t ->
  Bool
isSolvable constraints =
  \case
    Implicit _ t2 m ->
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
  , HasTypeIndexes k t
  ) =>
  [SolverConstraint k t] ->
  SolverChoice (SolverConstraint k t)
choice cs = findChoice [(delete c cs, c) | c <- cs]
 where
  findChoice ps =
    maybe NoneFound (uncurry Choice) (find (uncurry isSolvable) ps)

solveTypes ::
  ( Ord k
  , TypeSubstitutable (TypeConstraint TypeIndex k (Type TypeIndex k))
  , TypeSubstitutable (Type TypeIndex k)
  , TypeUnifiable (Type TypeIndex k)
  , MonadState Int m
  ) =>
  [SolverConstraint k (Type TypeIndex k)] ->
  m TypeSubstitution
solveTypes [] = pure (TypeSubstitution mempty)
solveTypes constraints =
  case choice constraints of
    NoneFound ->
      pure mempty
    Choice cs (Equality t1 t2) -> do
      sub1 <- unify t1 t2
      sub2 <- solveTypes (apply sub1 cs)
      pure (sub2 <> sub1)
    Choice cs (Implicit t1 t2 m) -> do
      solveTypes (Explicit t1 (generalize m t2) : cs)
    Choice cs (Explicit t1 s) -> do
      t2 <- instantiate s
      solveTypes (Equality t1 t2 : cs)

instantiate ::
  ( MonadState Int m
  , TypeSubstitutable (Type TypeIndex k)
  ) =>
  Scheme TypeIndex k (Type TypeIndex k) ->
  m (Type TypeIndex k)
instantiate (Forall qs ps t) = do
  sub <- foldrM go mempty qs
  pure (apply sub t)

-- where

go :: (MonadState Int m) => TypeIndex k -> TypeSubstitution -> m TypeSubstitution
go (TypeIndex _ index) a = do
  s <- get
  undefined
  pure (index `mapsTo` Type.Variable (TypeIndex undefined s) <> a)

generalize ::
  (HasTypeIndexes k t) =>
  MonomorphicSet (TypeIndex k) ->
  t ->
  Scheme TypeIndex k t
generalize (MonomorphicSet m) t = Forall (notBoundIn m (typeIndexesIn t)) [] t
