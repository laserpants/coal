{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystem.Constraint.SolverSpec where

import qualified Data.Set as Set
import Noll.Language.Type (Type)
import qualified Noll.Language.Type as Type
import Noll.Language.Type.Index (TypeIndex (..))
import qualified Noll.Language.Type.Intrinsic as Intrinsic
import Noll.TypeSystem.Substitution (TypeSubstitution (..))
import Noll.Language.Type.Kind (Kind)
import qualified Data.Map.Strict as Map
import qualified Noll.Language.Type.Kind as Kind
import Noll.TypeSystem.Constraint (MonomorphicSet (..), TypeConstraint (..))
import Noll.TypeSystem.Constraint.Solver
import Noll.TypeSystem.Unification (evalUnifier)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem.Constraint.Solver" $ do
    it "" $
      hasSubstitution
        fixture_1
        1 
        (typeBool `Type.Arrow` typeVariable 3)
    it "" $
      hasSubstitution
        fixture_1
        2 
        (typeBool `Type.Arrow` typeVariable 3)
    it "" $
      hasSubstitution
        fixture_1
        4 
        (typeVariable 3)
    it "" $
      hasSubstitution
        fixture_1
        5 
        (typeBool `Type.Arrow` typeVariable 3)

fixture_1 :: [TypeConstraint TypeIndex (Kind Int) (Type TypeIndex (Kind Int))]
fixture_1 =
  [ (Equality (typeVariable 2) (typeBool `Type.Arrow` typeVariable 3))
  , (Equality (typeVariable 5) (typeVariable 1))
  , (Equality (typeVariable 6) (typeVariable 1))
  , (Equality (typeVariable 7) (typeVariable 3))
  , (Implicit (typeVariable 4) (typeVariable 7) (MonomorphicSet (Set.fromList [TypeIndex Kind.Type 5])))
  , (Implicit (typeVariable 2) (typeVariable 6) (MonomorphicSet (Set.fromList [TypeIndex Kind.Type 5])))
  ]

hasSubstitution cs k s = Map.lookup k result == Just s
 where
  result =
    -- TODO
    typeSubstitutionMap (evalUnifier 100 (solveTypes cs))

typeVariable :: Int -> Type TypeIndex (Kind Int)
typeVariable = Type.Variable . TypeIndex Kind.Type

typeBool :: Type TypeIndex (Kind Int)
typeBool = Type.Intrinsic Intrinsic.Bool
