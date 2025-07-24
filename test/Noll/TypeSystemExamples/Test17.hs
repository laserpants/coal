{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystemExamples.Test17 where

import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Compiler
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

fixture :: Expression () ()
fixture =
  ELet
    ()
    ( BPattern
        ()
        ( PVariable
            ()
            ( Label
                () -- 0
                "f"
            )
        )
        ( ELambda
            ()
            ( PLiteral () (LInt32 5)
                <| PVariable () (Label () "x")
                  :| []
            )
            ( ELiteral () (LInt32 1)
            )
        )
        :| []
    )
    ( EApplication
        ()
        ()
        (EVariable () (Label () "f"))
        ( ELiteral () (LInt32 5)
            <| ELiteral () (LString "")
              :| []
        )
    )

fixtureR =
  ELet
    ()
    ( BPattern
        ()
        ( PVariable
            ()
            ( Label
                (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0))
                "f"
            )
        )
        (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0)) "g"))
        :| []
    )
    ( EApplication
        ()
        (TVariable (TypeIndex KType 1))
        ( EVariable
            ()
            ( Label
                (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1))
                "f"
            )
        )
        (ELiteral () (LInt32 5) :| [])
    )
