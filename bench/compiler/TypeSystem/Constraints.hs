{-# LANGUAGE OverloadedStrings #-}
{- |
Module: TypeSystem.Constraints
Description: Benchmarks for the type constraint solver and substitution.

These benchmarks exercise the constraint solver on synthetic, fully solvable
systems of increasing size, so that super-linear growth in the number of
constraints (the shape that large single definitions produce) shows up directly
in the benchmark output.

/Note:/ @Substitution@ has no @NFData@ instance, so every benchmark forces the
result through a cheap projection (the size of the substitution map, or a
depth-first forcing of a type) rather than through @nf@ on the substitution
itself.
-}
module TypeSystem.Constraints (benchmarks) where

import Coal.Language (
  IndexedType,
  Intrinsic (..),
  Kind (..),
  Type (..),
  TypeIndex (..),
 )
import Coal.TypeSystem.Constraint (Constraint (..))
import Coal.TypeSystem.Constraint.Generation.InferenceRule (InferenceRule (..))
import Coal.TypeSystem.Constraint.Solver (solveConstraints)
import Coal.TypeSystem.Substitution (Substitutable (..), Substitution (..), mapsTo)
import Criterion.Main
import qualified Data.Map.Strict as Map

-- | Constraint type specialised the way the compiler uses it.
type BenchConstraint = Constraint (InferenceRule Kind ()) TypeIndex Kind IndexedType

-- | A dummy inference rule; the solver only reports it on failure.
rule :: InferenceRule Kind ()
rule = RuleOperator ()

tvar :: Int -> IndexedType
tvar i = TVariable (TypeIndex KType i)

listOf :: IndexedType -> IndexedType
listOf t = TApplication KType (TConstructor (KArrow KType KType) "List") t

{- | A solvable system of @2 * n@ constraints shaped like the constraints a long
chain of function applications produces: the first half binds a variable to
@List<'k>@, the second half resolves those @'k@ to a concrete type. Every
constraint in the second half mentions a variable bound by the first half, so
substitutions have to propagate between constraints.
-}
mkChain :: Int -> [BenchConstraint]
mkChain n =
     [Equality rule [tvar i, listOf (tvar (n + i))] | i <- [0 .. n - 1]]
  <> [Equality rule [tvar (n + i), TIntrinsic IInt32] | i <- [0 .. n - 1]]

-- | Fresh variable supply large enough for 'mkChain'.
chainSupply :: Int -> Int
chainSupply n = 2 * n + 1

{- | A single list-literal constraint over @k@ distinct variables, shaped like
the @RuleListLiteral@ constraint a long list literal produces.
-}
mkListLiteral :: Int -> [BenchConstraint]
mkListLiteral k =
  let ts = fmap tvar [0 .. k - 1]
   in [Equality (RuleListLiteral () ts) ts]

-- | @List<List<…<int32>>>@ nested @d@ levels deep.
deepType :: Int -> IndexedType
deepType d = iterate listOf (TIntrinsic IInt32) !! d

-- | Solve a system and project the result to the number of bindings, so that
-- @criterion@ can force the solver's full result without needing @NFData@.
solveSize :: Int -> [BenchConstraint] -> Int
solveSize supply constraints =
  case solveConstraints supply constraints of
    (sub, _, _) -> Map.size (substitutionMap sub)

sizes :: [Int]
sizes = [100, 200, 400, 800]

depths :: [Int]
depths = [8, 16, 32, 64]

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup
      "solver"
      [ bgroup "chain" [bench ("n=" <> show n) $ nf (solveSize (chainSupply n)) (mkChain n) | n <- sizes]
      , bgroup
          "list-literal"
          [bench ("k=" <> show k) $ nf (solveSize (k + 1)) (mkListLiteral k) | k <- sizes]
      ]
  , bgroup
      "substitution"
      [ bench
          ("apply/depth-" <> show d)
          (nf (apply (mapsTo 0 (TIntrinsic IInt32))) (deepType d))
      | d <- depths
      ]
  ]