{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystem.ConstraintSolverSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Noll.Language (Intrinsic (..), Kind (..), KindIndex (..), Type (..), TypeIndex (..), freshIdIn)
import Noll.TypeSystem.ConstraintSolver (evalSolver, solveTypes)
import Noll.TypeSystem.TypeConstraint (MonomorphicSet (..), TypeConstraint (..))
import Noll.TypeSystem.TypeSubstitution (TypeSubstitution (..))
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem.ConstraintSolver" $ do
    it "" $
      hasSubstitutions
        fixture1
        [ (1, typeBool `TArrow` typeVariable 3)
        , (2, typeBool `TArrow` typeVariable 3)
        , (4, typeVariable 3)
        , (5, typeBool `TArrow` typeVariable 3)
        ]
    it "" $
      hasSubstitutions
        fixture2
        [ (1, typeVariable 3 `TArrow` typeVariable 3)
        , (2, typeVariable 3)
        , (4, typeInt32)
        , (5, typeInt32 `TArrow` typeInt32)
        , (6, (typeInt32 `TArrow` typeInt32) `TArrow` typeInt32 `TArrow` typeInt32)
        , (7, typeInt32 `TArrow` typeInt32)
        , (8, typeInt32)
        , (9, typeInt32 `TArrow` typeInt32)
        ]
    it "" $ hasNoErrors fixture1
    it "" $ hasNoErrors fixture2

-- fn(m) => let y = m in let x = y(true) in x
fixture1 :: [TypeConstraint () TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))]
fixture1 =
  [ (Equality () (typeVariable 2) (typeBool `TArrow` typeVariable 3))
  , (Equality () (typeVariable 5) (typeVariable 1))
  , (Equality () (typeVariable 6) (typeVariable 1))
  , (Equality () (typeVariable 7) (typeVariable 3))
  , (Implicit () (typeVariable 4) (typeVariable 7) (MonomorphicSet (Set.fromList [TypeIndex KType 5])))
  , (Implicit () (typeVariable 2) (typeVariable 6) (MonomorphicSet (Set.fromList [TypeIndex KType 5])))
  ]

-- let f = fn(x) => x in (f f)(f 1)
fixture2 :: [TypeConstraint () TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))]
fixture2 =
  [ (Implicit () (typeVariable 6) (typeVariable 1) (MonomorphicSet mempty))
  , (Implicit () (typeVariable 7) (typeVariable 1) (MonomorphicSet mempty))
  , (Implicit () (typeVariable 9) (typeVariable 1) (MonomorphicSet mempty))
  , (Equality () (typeVariable 2) (typeVariable 3))
  , (Equality () (typeVariable 6) (typeVariable 7 `TArrow` typeVariable 5))
  , (Equality () (typeVariable 9) (typeInt32 `TArrow` typeVariable 8))
  , (Equality () (typeVariable 1) (typeVariable 2 `TArrow` typeVariable 3))
  , (Equality () (typeVariable 5) (typeVariable 8 `TArrow` typeVariable 4))
  ]

hasSubstitutions :: [TypeConstraint () TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))] -> [(Int, Type TypeIndex (Kind KindIndex))] -> Bool
hasSubstitutions = all . uncurry . hasSubstitution

hasSubstitution :: [TypeConstraint () TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))] -> Int -> Type TypeIndex (Kind KindIndex) -> Bool
hasSubstitution cs k s = Map.lookup k (typeSubstitutionMap sub) == Just s
 where
  (sub, _) = evalSolver (freshIdIn cs) (solveTypes cs)

hasNoErrors :: [TypeConstraint () TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))] -> Bool
hasNoErrors cs = errors == []
 where
  (_, errors) = evalSolver (freshIdIn cs) (solveTypes cs)

typeVariable :: Int -> Type TypeIndex (Kind KindIndex)
typeVariable = TVariable . TypeIndex KType

typeBool :: Type TypeIndex k
typeBool = TIntrinsic IBool

typeInt32 :: Type TypeIndex k
typeInt32 = TIntrinsic IInt32
