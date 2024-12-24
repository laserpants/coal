{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystem.TypeConstraint.SolverSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Noll.Language.HasTypeIndexes (freshIdIn)
import Noll.Language.Type (Type)
import qualified Noll.Language.Type as Type
import Noll.Language.Type.Index (TypeIndex (..))
import qualified Noll.Language.Type.Intrinsic as Intrinsic
import Noll.Language.Type.Kind (Kind)
import qualified Noll.Language.Type.Kind as Kind
import Noll.Language.Type.Kind.Index (KindIndex (..))
import Noll.TypeSystem.TypeConstraint (MonomorphicSet (..), TypeConstraint (..))
import Noll.TypeSystem.TypeConstraint.Solver
import Noll.TypeSystem.TypeSubstitution (TypeSubstitution (..))
import Noll.TypeSystem.Unifier (evalUnifier)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem.TypeConstraint.Solver" $ do
    it "" $
      hasSubstitutions
        fixture_1
        [ (1, typeBool `Type.Arrow` typeVariable 3)
        , (2, typeBool `Type.Arrow` typeVariable 3)
        , (4, typeVariable 3)
        , (5, typeBool `Type.Arrow` typeVariable 3)
        ]
    -- TODO
    it "" $
      hasSubstitutions
        fixture_2
        [ (1, typeVariable 3 `Type.Arrow` typeVariable 3)
        , (2, typeVariable 3)
        , (4, typeInt32)
        , (5, typeInt32 `Type.Arrow` typeInt32)
        , (6, (typeInt32 `Type.Arrow` typeInt32) `Type.Arrow` typeInt32 `Type.Arrow` typeInt32)
        , (7, typeInt32 `Type.Arrow` typeInt32)
        , (8, typeInt32)
        , (9, typeInt32 `Type.Arrow` typeInt32)
        ]

-- fn(m) => let y = m in let x = y(true) in x
fixture_1 :: [TypeConstraint TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))]
fixture_1 =
  [ (Equality (typeVariable 2) (typeBool `Type.Arrow` typeVariable 3))
  , (Equality (typeVariable 5) (typeVariable 1))
  , (Equality (typeVariable 6) (typeVariable 1))
  , (Equality (typeVariable 7) (typeVariable 3))
  , (Implicit (typeVariable 4) (typeVariable 7) (MonomorphicSet (Set.fromList [TypeIndex Kind.Type 5])))
  , (Implicit (typeVariable 2) (typeVariable 6) (MonomorphicSet (Set.fromList [TypeIndex Kind.Type 5])))
  ]

-- let f = fn(x) => x in (f f)(f 1)
fixture_2 :: [TypeConstraint TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))]
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

hasSubstitutions :: [SolverConstraint (Kind KindIndex) (Type TypeIndex (Kind KindIndex))] -> [(Int, Type TypeIndex (Kind KindIndex))] -> Bool
hasSubstitutions = all . uncurry . hasSubstitution

hasSubstitution :: [SolverConstraint (Kind KindIndex) (Type TypeIndex (Kind KindIndex))] -> Int -> Type TypeIndex (Kind KindIndex) -> Bool
hasSubstitution cs k s = Map.lookup k result == Just s
 where
  result =
    typeSubstitutionMap (evalUnifier (freshIdIn cs) (solveTypes cs))

typeVariable :: Int -> Type TypeIndex (Kind KindIndex)
typeVariable = Type.Variable . TypeIndex Kind.Type

typeBool :: Type TypeIndex k
typeBool = Type.Intrinsic Intrinsic.Bool

typeInt32 :: Type TypeIndex k
typeInt32 = Type.Intrinsic Intrinsic.Int32
