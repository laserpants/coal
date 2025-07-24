{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystemExamples.Test03 where

import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Compiler2
import Noll.Language (
  BinaryOperator (..),
  Binding (..),
  Choice (..),
  Clause (..),
  Constructor (..),
  Expression (..),
  Intrinsic (..),
  Kind (..),
  Parameter (..),
  Pattern (..),
  Primitive (..),
  Row (..),
  Scheme (..),
  Type (..),
  TypeIndex (..),
 )
import Noll.TypeSystemSpec.TestRunner
import Test.Hspec (Spec, describe, hspec, it)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Lang.Common.Environment as Environment

spec :: Spec
spec =
  describe "let in_range = fn({ max, min } : Range(a), n : a) => gt(n, min) && (gt(min, max) || lte(n, max)) in in_range" $
    it "" $ do
      testResultExpression (runTest fixture)
        == ( ELet
              ()
              ( BPattern
                  ()
                  ( PVariable
                      ()
                      ( Label
                          ( TIntrinsic
                              ( IRecord
                                  (TRow (RExtend "max" (TVariable (TypeIndex KType 0)) (RExtend "min" (TVariable (TypeIndex KType 0)) RNil)))
                              )
                              `TArrow` TVariable (TypeIndex KType 0)
                              `TArrow` TIntrinsic IBool
                          )
                          "in_range"
                      )
                  )
                  ( ELambda
                      ()
                      ( PAnnotation
                          ()
                          ( TAlias
                              "Range"
                              [TVariable (Parameter () "a")]
                              ( TIntrinsic
                                  ( IRecord
                                      (TRow (RExtend "max" (TVariable (Parameter () "a")) (RExtend "min" (TVariable (Parameter () "a")) RNil)))
                                  )
                              )
                          )
                          ( PRecord
                              ()
                              ( TIntrinsic
                                  ( IRecord
                                      (TRow (RExtend "max" (TVariable (TypeIndex KType 0)) (RExtend "min" (TVariable (TypeIndex KType 0)) RNil)))
                                  )
                              )
                              ( Map.fromList
                                  [
                                    ( "max"
                                    , PShorthand () (Label (TVariable (TypeIndex KType 0)) "max")
                                    )
                                  ,
                                    ( "min"
                                    , PShorthand () (Label (TVariable (TypeIndex KType 0)) "min")
                                    )
                                  ]
                              )
                              Nothing
                          )
                          <| PAnnotation () (TVariable (Parameter () "a")) (PVariable () (Label (TVariable (TypeIndex KType 0)) "n"))
                            :| []
                      )
                      ( EApplication
                          ()
                          (TIntrinsic IBool)
                          ( EBinaryOperator
                              ()
                              ( TIntrinsic IBool
                                  `TArrow` TIntrinsic IBool
                                  `TArrow` TIntrinsic IBool
                              )
                              OLogicalAnd
                          )
                          ( EApplication
                              ()
                              (TIntrinsic IBool)
                              ( EVariable
                                  ()
                                  ( Label
                                      ( TVariable (TypeIndex KType 0)
                                          `TArrow` TVariable (TypeIndex KType 0)
                                          `TArrow` TIntrinsic IBool
                                      )
                                      "gt"
                                  )
                              )
                              (EVariable () (Label (TVariable (TypeIndex KType 0)) "n") <| EVariable () (Label (TVariable (TypeIndex KType 0)) "min") :| [])
                              <| EApplication
                                ()
                                (TIntrinsic IBool)
                                ( EBinaryOperator
                                    ()
                                    ( TIntrinsic IBool
                                        `TArrow` TIntrinsic IBool
                                        `TArrow` TIntrinsic IBool
                                    )
                                    OLogicalOr
                                )
                                ( EApplication
                                    ()
                                    (TIntrinsic IBool)
                                    ( EVariable
                                        ()
                                        ( Label
                                            ( TVariable (TypeIndex KType 0)
                                                `TArrow` TVariable (TypeIndex KType 0)
                                                `TArrow` TIntrinsic IBool
                                            )
                                            "gt"
                                        )
                                    )
                                    (EVariable () (Label (TVariable (TypeIndex KType 0)) "min") <| EVariable () (Label (TVariable (TypeIndex KType 0)) "max") :| [])
                                    <| EApplication
                                      ()
                                      (TIntrinsic IBool)
                                      ( EVariable
                                          ()
                                          ( Label
                                              ( TVariable (TypeIndex KType 0)
                                                  `TArrow` TVariable (TypeIndex KType 0)
                                                  `TArrow` TIntrinsic IBool
                                              )
                                              "lte"
                                          )
                                      )
                                      (EVariable () (Label (TVariable (TypeIndex KType 0)) "n") <| EVariable () (Label (TVariable (TypeIndex KType 0)) "max") :| [])
                                      :| []
                                )
                                :| []
                          )
                      )
                  )
                  :| []
              )
              ( EVariable
                  ()
                  ( Label
                      ( TIntrinsic
                          ( IRecord
                              (TRow (RExtend "max" (TVariable (TypeIndex KType 1)) (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)))
                          )
                          `TArrow` TVariable (TypeIndex KType 1)
                          `TArrow` TIntrinsic IBool
                      )
                      "in_range"
                  )
              )
           )

runTest :: (Show a, Eq a, Data a) => Expression a () -> TestResult (Expression a (Type TypeIndex Kind)) a
runTest =
  runTypedExpressionTest
    (CompilerEnvironment env1 env2 env3)
    [
      ( "lte"
      , Forall
          (Set.fromList [TypeIndex KType 0])
          []
          ( TVariable (TypeIndex KType 0)
              `TArrow` TVariable (TypeIndex KType 0)
              `TArrow` TIntrinsic IBool
          )
      )
    ,
      ( "gt"
      , Forall
          (Set.fromList [TypeIndex KType 0])
          []
          ( TVariable (TypeIndex KType 0)
              `TArrow` TVariable (TypeIndex KType 0)
              `TArrow` TIntrinsic IBool
          )
      )
    ]
 where
  env1 =
    Environment.fromList
      []
  env2 =
    Environment.fromList
      []
  env3 =
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
                      [TVariable (Parameter () "a")]
                      ( TIntrinsic
                          ( IRecord
                              (TRow (RExtend "max" (TVariable (Parameter () "a")) (RExtend "min" (TVariable (Parameter () "a")) RNil)))
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
                            , PShorthand () (Label () "min")
                            )
                          ]
                      )
                      Nothing
                  )
                  <| PAnnotation () (TVariable (Parameter () "a")) (PVariable () (Label () "n"))
                    :| []
              )
              ( EApplication
                  ()
                  ()
                  (EBinaryOperator () () OLogicalAnd)
                  ( EApplication
                      ()
                      ()
                      (EVariable () (Label () "gt"))
                      (EVariable () (Label () "n") <| EVariable () (Label () "min") :| [])
                      <| EApplication
                        ()
                        ()
                        (EBinaryOperator () () OLogicalOr)
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
