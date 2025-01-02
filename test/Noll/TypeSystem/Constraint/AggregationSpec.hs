{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystem.Constraint.AggregationSpec where

import Data.List.NonEmpty (NonEmpty (..))
import Noll.Label (Label (..))
import Noll.Language (Binding (..), Expression (..), Kind (..), KindIndex, Pattern (..), Primitive (..), Type (..), TypeIndex (..))
import Noll.TypeSystem.Constraint.Aggregation
import Noll.TypeSystem.Constraint.Rule (Assumption (..))
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem.Constraint.Aggregation" $ do
    describe "ELet" $ do
      it "" $
        1 == 0

testCase ::
  Expression a Int ->
  ( [Assumption (Type TypeIndex (Kind KindIndex))]
  , [AggregationOutput a TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))]
  )
testCase e =
  let
    e1 =
      toIndexed e
   in
    runAggregationStack
      (AggregationContext mempty mempty)
      (aggregateConstraints e1)

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
