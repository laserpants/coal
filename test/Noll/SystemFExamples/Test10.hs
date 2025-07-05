{-# LANGUAGE OverloadedStrings #-}

module Noll.SystemFExamples.Test10 where

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
  Pattern (..),
  Primitive (..),
  Scheme (..),
  Type (..),
  TypeIndex (..),
  With (..),
 )
import Noll.Module (Constant (..), Function (..), Module (..))
import Noll.SystemFSpec.TestRunner
import Test.Hspec (Spec, describe, hspec, it)

import qualified Data.Set as Set
import qualified Lang.Common.Environment as Environment

spec :: Spec
spec =
  describe "gt(x) = not <<< lte(x)" $
    it "" $ do
      testResultExpression (runTest fixture)
        == fixture1

runTest :: (Show a, Eq a, Data a) => Function Expression a () -> TestResult (Function Expression a (Type TypeIndex Kind)) a
runTest =
  runTypedFunctionTest
    (CompilerEnvironment env1 env2 env3)
    [
      ( "not"
      , Forall
          mempty
          []
          (TIntrinsic IBool `TArrow` TIntrinsic IBool)
      )
    ,
      ( "lte"
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
      [ ("Ordering", KType)
      ]
  env3 =
    Environment.fromList
      []

--
-- gt(x) = not <<< lte(x)
--
fixture :: Function Expression () ()
fixture =
  Function
    ()
    (With [] ())
    (PVariable () (Label () "x") :| [])
    ( EApplication
        ()
        ()
        (EBinaryOperator () () OReverseComposition)
        ( EVariable () (Label () "not")
            <| EApplication
              ()
              ()
              (EVariable () (Label () "lte"))
              (EVariable () (Label () "x") :| [])
            :| []
        )
    )

fixture1 :: Function Expression () (Type TypeIndex Kind)
fixture1 =
  Function
    ()
    (With [] (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool))
    (PVariable () (Label (TVariable (TypeIndex KType 0)) "x") :| [])
    ( EApplication
        ()
        (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
        (EBinaryOperator () ((TIntrinsic IBool `TArrow` TIntrinsic IBool) `TArrow` (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) OReverseComposition)
        ( EVariable () (Label (TIntrinsic IBool `TArrow` TIntrinsic IBool) "not")
            <| EApplication
              ()
              (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
              (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "lte"))
              (EVariable () (Label (TVariable (TypeIndex KType 0)) "x") :| [])
            :| []
        )
    )
