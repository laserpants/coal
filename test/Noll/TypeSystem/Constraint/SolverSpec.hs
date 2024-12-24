{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystem.Constraint.SolverSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Noll.Language.Type (Type)
import qualified Noll.Language.Type as Type
import Noll.Language.Type.Index (TypeIndex (..))
import qualified Noll.Language.Type.Intrinsic as Intrinsic
import Noll.Language.Type.Kind (Kind)
import qualified Noll.Language.Type.Kind as Kind
import Noll.TypeSystem.Constraint (MonomorphicSet (..), TypeConstraint (..))
import Noll.TypeSystem.Constraint.Solver
import Noll.TypeSystem.Substitution (TypeSubstitution (..))
import Noll.TypeSystem.Unification (evalUnifier)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem.Constraint.Solver" $ do
    it "" $
      hasSubstitutions
        fixture_1
        [ (1, typeBool `Type.Arrow` typeVariable 3)
        , (2, typeBool `Type.Arrow` typeVariable 3)
        , (4, typeVariable 3)
        , (5, typeBool `Type.Arrow` typeVariable 3)
        ]

-- fn(m) => let y = m in let x = y(true) in x
fixture_1 :: [TypeConstraint TypeIndex (Kind Int) (Type TypeIndex (Kind Int))]
fixture_1 =
  [ (Equality (typeVariable 2) (typeBool `Type.Arrow` typeVariable 3))
  , (Equality (typeVariable 5) (typeVariable 1))
  , (Equality (typeVariable 6) (typeVariable 1))
  , (Equality (typeVariable 7) (typeVariable 3))
  , (Implicit (typeVariable 4) (typeVariable 7) (MonomorphicSet (Set.fromList [TypeIndex Kind.Type 5])))
  , (Implicit (typeVariable 2) (typeVariable 6) (MonomorphicSet (Set.fromList [TypeIndex Kind.Type 5])))
  ]

fixture_2 :: [TypeConstraint TypeIndex (Kind Int) (Type TypeIndex (Kind Int))]
fixture_2 =
  [ (Implicit (typeVariable 6) (typeVariable 1) (MonomorphicSet mempty))
  , (Implicit (typeVariable 7) (typeVariable 1) (MonomorphicSet mempty))
  , (Implicit (typeVariable 9) (typeVariable 1) (MonomorphicSet mempty))
  , (Equality (typeVariable 2) (typeVariable 3))
  , (Equality (typeVariable 6) (typeVariable 7 `Type.Arrow` typeVariable 5))
  , (Equality (typeVariable 9) (typeInt32 `Type.Arrow` typeVariable 8))
  , (Equality (typeVariable 1) (typeVariable 2 `Type.Arrow` typeVariable 3))
  , (Equality (typeVariable 5) (typeVariable 8 `Type.Arrow` typeVariable 4))
  ]

hasSubstitutions :: [SolverConstraint (Kind Int) (Type TypeIndex (Kind Int))] -> [(Int, Type TypeIndex (Kind Int))] -> Bool
hasSubstitutions = all . uncurry . hasSubstitution

hasSubstitution :: [SolverConstraint (Kind Int) (Type TypeIndex (Kind Int))] -> Int -> Type TypeIndex (Kind Int) -> Bool
hasSubstitution cs k s = Map.lookup k result == Just s
 where
  result =
    -- TODO
    typeSubstitutionMap (evalUnifier 100 (solveTypes cs))

typeVariable :: Int -> Type TypeIndex (Kind Int)
typeVariable = Type.Variable . TypeIndex Kind.Type

typeBool :: Type TypeIndex k
typeBool = Type.Intrinsic Intrinsic.Bool

typeInt32 :: Type TypeIndex k
typeInt32 = Type.Intrinsic Intrinsic.Int32
