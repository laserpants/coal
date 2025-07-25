{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystemExamples.Test16 where

import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Lang.Common.Label (Label (..))
import Noll.Compiler2
import Noll.Language (
  Binding (..),
  Choice (..),
  Clause (..),
  Constructor (..),
  Expression (..),
  Intrinsic (..),
  Kind (..),
  Pattern (..),
  Primitive (..),
  Scheme (..),
  Type (..),
  TypeIndex (..),
 )
import Noll.TypeSystemSpec.TestRunner
import Test.Hspec (Spec, describe, it)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Lang.Common.Environment as Environment

spec :: Spec
spec =
  describe "??" $
    it "" $ do
      testResultExpression (runTest fixture)
        == fixtureR

runTest :: (Show a, Eq a, Data a) => Expression a () -> TestResult (Expression a (Type TypeIndex Kind)) a
runTest =
  runTypedExpressionTest
    (CompilerEnvironment env1 env2 env3)
    [
      ( "compare"
      , Forall
          (Set.fromList [TypeIndex KType 0])
          []
          ( TVariable (TypeIndex KType 0)
              `TArrow` TVariable (TypeIndex KType 0)
              `TArrow` TConstructor KType "Ordering"
          )
      )
    ]
 where
  env1 =
    Environment.fromList
      [
        ( "EqualTo"
        , Constructor
            "EqualTo"
            0
            (Forall mempty [] (TConstructor KType "Ordering"))
        )
      ,
        ( "GreaterThan"
        , Constructor
            "GreaterThan"
            0
            (Forall mempty [] (TConstructor KType "Ordering"))
        )
      ,
        ( "LessThan"
        , Constructor
            "LessThan"
            0
            (Forall mempty [] (TConstructor KType "Ordering"))
        )
      ]
  env2 =
    Environment.fromList
      [ ("Ordering", KType)
      ]
  env3 =
    Environment.fromList
      []

--
-- let
--   lte =
--     fn(x) =>
--        fn(y) =>
--          match(compare(x, y)) {
--            | LessThan or EqualTo => true
--            | GreaterThan => false
--   in
--     lte
--
fixture :: Expression () ()
fixture =
  ( ERecursiveLet
      ()
      (PVariable () (Label () "unfold"))
      ( ELambda
          ()
          (PVariable () (Label () "n") :| [])
          ( ERecord
              ()
              ()
              ( Map.fromList
                  [
                    ( "abc"
                    , EVariable () (Label () "unfold")
                    )
                  ]
              )
              Nothing
          )
      )
      (EVariable () (Label () "unfold"))
  )

fixtureR =
  ( ERecursiveLet
      undefined
      (PVariable () (Label undefined "unfold"))
      ( ELambda
          ()
          (PVariable () (Label undefined "n") :| [])
          ( ERecord
              ()
              undefined
              ( Map.fromList
                  [
                    ( "abc"
                    , ELiteral () (LInt32 1)
                    )
                  ]
              )
              Nothing
          )
      )
      (EVariable () (Label undefined "unfold"))
  )
