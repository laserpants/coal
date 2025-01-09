{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystemExamples.Test3 where

import Data.List.NonEmpty (NonEmpty (..), (<|))
import Noll.Compiler
import Noll.Label (Label (..))
import Noll.Language (
  BinaryOperator (..),
  Binding (..),
  Choice (..),
  Clause (..),
  Constructor (..),
  Expression (..),
  Intrinsic (..),
  Kind (..),
  Pattern (..),
  Primitive (..),
  Row (..),
  Scheme (..),
  Type (..),
  TypeIndex (..),
  TypeParam (..),
 )
import Noll.TypeSystemSpec.TestRunner
import Test.Hspec (Spec, describe, hspec, it)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Noll.Lib.Environment as Environment

spec :: Spec
spec =
  describe "" $
    it "" $ do
      runTest fixture
        == undefined

runTest :: (Eq a) => Expression a () -> Expression a (Type TypeIndex Kind)
runTest =
  testResultExpression
    . runTypedExpressionTest
      (CompilerEnvironment env1 env2)
      []
 where
  env1 =
    Environment.fromList
      []
  env2 =
    Environment.fromList
      []

--
-- let
--   in_range =
--     fn({ max, min } : Range(a), n : a) =>
--       gt(n, min) &&
--         (gt(min, max) || lte(n, max))
--   in
--     in_range
--
fixture :: Expression () ()
fixture =
  ( ELet
      ()
      ( BPattern
          ()
          (PVariable () (Label () "in_range"))
          ( ELambda
              ()
              ( PAnnotation
                  ()
                  ( TAlias
                      "Range"
                      [TVariable (TypeParam () "a")]
                      ( TIntrinsic
                          ( IRecord
                              (TRow (RExtend "min" (TVariable (TypeParam () "a")) (RExtend "max" (TVariable (TypeParam () "a")) RNil)))
                          )
                      )
                  )
                  ( PRecord
                      ()
                      ()
                      ( Map.fromList
                          [
                            ( "max"
                            , PShorthand () (Label () "max")
                            )
                          ,
                            ( "min"
                            , PShorthand () (Label () "max")
                            )
                          ]
                      )
                      Nothing
                  )
                  <| PAnnotation () (TVariable (TypeParam () "a")) (PVariable () (Label () "n"))
                    :| []
              )
              ( EApplication
                  ()
                  ()
                  (EBinaryOperator () ((), OLogicalAnd))
                  ( EApplication
                      ()
                      ()
                      (EVariable () (Label () "gt"))
                      (EVariable () (Label () "n") <| EVariable () (Label () "min") :| [])
                      <| EApplication
                        ()
                        ()
                        (EBinaryOperator () ((), OLogicalOr))
                        ( EApplication
                            ()
                            ()
                            (EVariable () (Label () "gt"))
                            (EVariable () (Label () "min") <| EVariable () (Label () "max") :| [])
                            <| EApplication
                              ()
                              ()
                              (EVariable () (Label () "lte"))
                              (EVariable () (Label () "n") <| EVariable () (Label () "max") :| [])
                              :| []
                        )
                        :| []
                  )
              )
          )
          :| []
      )
      (EVariable () (Label () "in_range"))
  )
