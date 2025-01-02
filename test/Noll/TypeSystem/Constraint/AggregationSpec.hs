{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystem.Constraint.AggregationSpec where

import Data.List.NonEmpty (NonEmpty (..))
import Noll.Label (Label (..))
import Noll.Language (Binding (..), Expression (..), Kind (..), Pattern (..), Primitive (..), TypeIndex (..))
import Noll.TypeSystem.Constraint.Aggregation
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem.Constraint.Aggregation" $ do
    it "" $
      1 == 0

-- testCase :: Expression String Int ->
testCase e =
  let
    e1 =
      fmap (TypeIndex KType) e
   in
    undefined

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
