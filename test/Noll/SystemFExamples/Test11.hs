{-# LANGUAGE OverloadedStrings #-}

module Noll.SystemFExamples.Test11 where

import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Compiler
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
  With (..),
 )
import Noll.Module (Constant (..), Function (..), Module (..))
import Noll.SystemFSpec.TestRunner
import Test.Hspec (Spec, describe, hspec, it)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Lang.Common.Environment as Environment

spec :: Spec
spec =
  describe "" $
    it "" $ do
      testResultExpression (runTest fixture)
        == fixture1

runTest :: (Show a, Eq a, Data a) => Function Expression a () -> TestResult (Function Expression a (Type TypeIndex Kind)) a
runTest =
  runTypedFunctionTest
    (CompilerEnvironment env1 env2)
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

--
-- in_range({ max, min } : Range(a), n : a) =
--   gt(n, min) && (gt(min, max) || lte(n, max))
--
fixture :: Function Expression () ()
fixture =
  Function
    ()
    (With [] ())
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

fixture1 :: Function Expression () (Type TypeIndex Kind)
fixture1 =
  Function
    ()
    (With [] (TIntrinsic IBool))
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
