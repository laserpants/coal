{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint.Solver where

import Data.List (delete, find)
import Data.Set (intersection, (\\))
import qualified Data.Set as Set
import Noll.Language.HasActive (activeIdsIn)
import Noll.Language.HasTypeIndexes (HasTypeIndexes, typeIdsIn)
import Noll.Language.Type.Index (TypeIndex (..))
import Noll.TypeSystem.Constraint (MonomorphicSet (..), TypeConstraint (..))

isSolvable ::
  (HasTypeIndexes () t) =>
  [TypeConstraint TypeIndex () t] ->
  TypeConstraint TypeIndex () t ->
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
  (Eq t, HasTypeIndexes () t) =>
  [TypeConstraint TypeIndex () t] ->
  SolverChoice (TypeConstraint TypeIndex () t)
choice cs = findChoice [(delete c cs, c) | c <- cs]
 where
  findChoice ps = maybe NoneFound (uncurry Choice) (find (uncurry isSolvable) ps)
