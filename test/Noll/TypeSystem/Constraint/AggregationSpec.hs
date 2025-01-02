{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystem.Constraint.AggregationSpec where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Noll.Label (Label (..))
import Noll.Language (
  Binding (..),
  Expression (..),
  Kind (..),
  KindIndex,
  Pattern (..),
  Primitive (..),
  Scheme (..),
  Type (..),
  TypeIndex (..),
  TypeParam (..),
 )
import Noll.TypeSystem.Constraint.Aggregation
import Noll.TypeSystem.Constraint.Rule (Assumption (..))
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem.Constraint.Aggregation" $ do
    --    describe "aggregateConstraints" $ do
    --      describe "ELet" $ do
    --        it "" $
    --          1 == 0
    describe "instantiateAnnotation" $ do
      it "" $
        testCase2
          (TArrow (TVariable (TypeParam () "a")) (TVariable (TypeParam () "b")))
          == Just (Forall (Set.fromList [TypeIndex KType 0, TypeIndex KType 1]) [] (TArrow (TVariable (TypeIndex KType 0)) (TVariable (TypeIndex KType 1))))

testCase ::
  Expression a Int ->
  ( [Assumption (Type TypeIndex (Kind KindIndex))]
  , [AggregationOutput a TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))]
  )
testCase e =
  runAggregationStack
    (AggregationContext mempty mempty mempty)
    (aggregateConstraints (toIndexed e))

testCase2 :: Type TypeParam () -> Maybe (Scheme TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex)))
testCase2 t = s
 where
  (s, _) =
    runAggregationStack
      (AggregationContext mempty mempty mempty)
      (instantiateAnnotation t)

toIndexed :: Expression a Int -> Expression a (Type TypeIndex (Kind KindIndex))
toIndexed = fmap (TVariable . TypeIndex KType)

-- let x = 5 in x
fixture7 :: Expression String Int
fixture7 =
  ELet
    "ELet"
    ( BPattern
        "BPattern"
        (PVariable "PVariable" (Label 1 "x"))
        (ELiteral "ELiteral" (LInt32 5))
        :| []
    )
    (EVariable "EVariable" (Label 2 "x"))

-- let x = 5 in y
fixture8 :: Expression String Int
fixture8 =
  ELet
    "ELet"
    ( BPattern
        "BPattern"
        (PVariable "PVariable" (Label 1 "x"))
        (ELiteral "ELiteral" (LInt32 5))
        :| []
    )
    (EVariable "EVariable" (Label 2 "y"))

-- let x = 5 in 1
fixture9 :: Expression String Int
fixture9 =
  ELet
    "ELet"
    ( BPattern
        "BPattern"
        (PVariable "PVariable" (Label 1 "x"))
        (ELiteral "ELiteral" (LInt32 5))
        :| []
    )
    (ELiteral "ELiteral" (LInt32 1))
