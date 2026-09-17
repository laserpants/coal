{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.TypeSystem.Constraint.Solver
Description: Constraint solving via unification and substitution

This module implements the constraint solver for type inference. It takes
the constraints generated during the constraint generation phase and solves
them using unification and substitution to produce concrete type assignments.
The solver handles equality constraints, implicit generalization, explicit
instantiation, and row polymorphism constraints.
-}
module Coal.TypeSystem.Constraint.Solver (
  Solver (..),
  runSolver,
  solve,
  solveConstraints,
) where

import Coal.Common.Supply (supply)
import Coal.Language (
  IndexedScheme,
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
import Coal.TypeSystem.Constraint (Constraint (..), Monomorphic (..))
import Coal.TypeSystem.Substitution (Substitutable (..), Substitution (..), mapsTo)
import Coal.TypeSystem.Unification (UnificationError, Unifier (..), runUnifier, unifyAll)
import Control.Monad.RWS.Strict (MonadState, MonadWriter, RWS, get, put, runRWS, tell)
import Data.List (delete, find, foldl')
import qualified Data.Map.Strict as Map
import Data.Set (Set, intersection, (\\))
import qualified Data.Set as Set
import Extras (foldrM)

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
solveConstraints :: (Eq s) => Int -> [SolverConstraint s] -> (Substitution, Int, [s])
solveConstraints n cs = runSolver n (solve cs)

{-# INLINE runSolver #-}
runSolver :: Int -> Solver s t -> (t, Int, [s])
runSolver n s = runRWS (solverMonad s) () n

{- | A solver constraint together with the type variable sets used to drive
incremental substitution application.

'entryFreeVars' is the set of type variable identifiers occurring anywhere in
the constraint, and 'entryActiveVars' is the constraint's @HasActive@ set, i.e.
the set the solver consults when deciding whether an implicit constraint may be
solved yet. Both are cached alongside the constraint so that re-application of a
substitution and solvability checks never have to re-traverse the whole
constraint list.
-}
data SolverEntry s = SolverEntry
  { entryConstraint :: SolverConstraint s
  , entryFreeVars :: Set Int
  , entryActiveVars :: Set Int
  }
  deriving (Show, Eq, Ord, Read)

mkEntry :: SolverConstraint s -> SolverEntry s
mkEntry c = SolverEntry c (typeIdsIn c) (activeIdsIn c)

{- | Union of the active variable sets of every constraint in the list. This is
the same set as @activeIdsIn@ applied to the constraints themselves, computed
from the cached per-entry sets instead of by traversing the constraints again.
-}
entriesActiveIds :: [SolverEntry s] -> Set Int
entriesActiveIds = Set.unions . fmap entryActiveVars

isSolvable :: [SolverEntry s] -> SolverConstraint s -> Bool
isSolvable entries =
  \case
    Implicit _ _ t2 m ->
      Set.null (typeIdsIn t2 \\ typeIdsIn m `intersection` entriesActiveIds entries)
    _ ->
      True

data SolverChoice e c = Choice [e] c | ChoiceNotFound
  deriving (Show, Eq, Ord, Read)

choice :: (Eq s) => [SolverEntry s] -> SolverChoice (SolverEntry s) (SolverConstraint s)
choice es = findChoice [(delete e es, entryConstraint e) | e <- es]
 where
  findChoice =
    maybe ChoiceNotFound (uncurry Choice) . find (\(rest, c) -> isSolvable rest c)

{- | Apply a freshly discovered substitution to the constraints it can affect.

A constraint that mentions none of the variables bound by the substitution is
left untouched: substituting such a constraint is the identity on its
structure, so dropping the rewrite cannot change the result of solving. Only
the constraints that do mention one of the bound variables are re-substituted,
which keeps the cost of solving proportional to the number of constraints that
are actually affected rather than to the size of the remaining constraint set.
-}
applySubstitution :: Substitution -> [SolverEntry s] -> [SolverEntry s]
applySubstitution sub
  | Map.null (substitutionMap sub) = id
  | otherwise = fmap applyTo
 where
  keys = Map.keysSet (substitutionMap sub)
  applyTo e
    | Set.null (entryFreeVars e `Set.intersection` keys) = e
    | otherwise = mkEntry (apply sub (entryConstraint e))

solve :: (Eq s) => [SolverConstraint s] -> Solver s Substitution
solve = go [] . fmap mkEntry
 where
  {- Compose fragments oldest-first so the result matches the original
  recursive solver's @sub2 <> sub1@ nesting: with fragments stored most
  recent first (@sub1 : frags@ at each 'Equality' step), 'foldr' applies them
  oldest-first, rebuilding exactly that nesting. -}
  go frags [] = pure (composeSubstitutions frags)
  go frags entries =
    case choice entries of
      ChoiceNotFound ->
        pure (composeSubstitutions frags)
      Choice rest (Equality c ts) -> do
        res <- transUnifier (unifyAll ts)
        case res of
          Left{} -> do
            tell [c]
            go frags rest
          Right sub1 ->
            go (sub1 : frags) (applySubstitution sub1 rest)
      Choice rest (Implicit c t1 t2 m) ->
        go frags (mkEntry (Explicit c t1 (generalize m t2)) : rest)
      Choice rest (Explicit c t1 s) -> do
        t2 <- instantiate s
        go frags (mkEntry (Equality c [t1, t2]) : rest)
      Choice rest Lacks{} ->
        go frags rest

{- | Combine the substitution fragments produced while solving.

Fragments arrive most recent first; the nested composition they replace,
@foldr (<>) mempty frags@, puts the newest fragment outermost, i.e. newer
fragments are applied to the maps of older fragments (and to each other in
solving order, oldest first).

This function computes the same composition incrementally, walking the
fragments in solving order (oldest first) and maintaining the composition of
the fragments processed so far. A fragment's map rewrites every accumulated
binding whose type mentions one of the fragment's bound variables — exactly
what the nested @apply newer (map older)@ does — while bindings that avoid
the new variables cannot be affected (@apply@ is the identity on types that
do not mention a bound variable) and are kept unchanged. The fragment's own
bindings are added as they are; on the (not expected to occur) key collision
with an older binding, the rewritten older entry wins, matching the
left-biased union in 'Semigroup'.

The result is the same substitution the nested composition yields, but its
cost is proportional to the number of bindings actually affected rather than
to (fragments × accumulated map size).
-}

-- An accumulated binding in 'composeSubstitutions': its composed value plus
-- the ids of the type variables occurring in it, used to skip fragments that
-- cannot affect it.
type ComposedEntry = (Set Int, IndexedType)

composeSubstitutions :: [Substitution] -> Substitution
composeSubstitutions =
  Substitution . Map.map snd . foldl' addFragment Map.empty . reverse
 where
  addFragment :: Map.Map Int ComposedEntry -> Substitution -> Map.Map Int ComposedEntry
  addFragment acc sub@(Substitution m)
    | Map.null m = acc
    | otherwise = Map.union rewritten ownBindings
   where
    keys = Map.keysSet m
    rewritten = Map.map passThrough acc
    passThrough entry@(freeVars, value)
      | Set.null (freeVars `Set.intersection` keys) =
          entry
      | otherwise =
          let value' = apply sub value
           in (typeIdsIn value', value')
    ownBindings = fmap (\value -> (typeIdsIn value, value)) m

{-# INLINE generalize #-}
generalize :: (TypeIndexed k t) => Monomorphic (TypeIndex k) -> t -> Scheme TypeIndex k t
generalize (Monomorphic m) t = Forall (notBoundIn m (typeIndexesIn t)) mempty t

instantiate :: IndexedScheme -> Solver s IndexedType
instantiate (Forall qs _ t) = do
  sub <- foldrM go mempty qs
  pure (apply sub t)
 where
  go (TypeIndex k index) sub = do
    s <- supply
    pure (index `mapsTo` TVariable (TypeIndex k s) <> sub)
