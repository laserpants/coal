{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystemExamples.Test9 where

import Data.List.NonEmpty (NonEmpty (..), (<|))
import Noll.Compiler
import Noll.Label (Label (..))
import Noll.Language (
  Binding (..),
  Choice (..),
  Clause (..),
  Constructor (..),
  Expression (..),
  Function (..),
  Intrinsic (..),
  Kind (..),
  Object (..),
  Pattern (..),
  Primitive (..),
  Scheme (..),
  Type (..),
  TypeIndex (..),
  Uses (..),
 )
import Noll.TypeSystemSpec.TestRunner
import Test.Hspec (Spec, describe, hspec, it)

import qualified Data.Set as Set
import qualified Noll.Lib.Environment as Environment

spec :: Spec
spec =
  describe "" $
    it "" $ do
      1 == 2 -- undefined

runTest :: (Show a, Eq a) => Expression a () -> TestResult a
runTest =
  runTypedExpressionTest
    (CompilerEnvironment env1 env2)
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

--
-- lte(x) =
--   fn(y) =>
--     match(compare(x, y)) {
--       | LessThan or EqualTo => true
--       | GreaterThan => false
--
fixture :: Object () () ()
fixture =
  DFunction
    ()
    "lte"
    ( Function
        (Uses [] ())
        (PVariable () (Label () "x") :| [])
        ( ELambda
            ()
            (PVariable () (Label () "y") :| [])
            ( EMatch
                ()
                ()
                ( EApplication
                    ()
                    ()
                    (EVariable () (Label () "compare"))
                    (EVariable () (Label () "x") <| EVariable () (Label () "y") :| [])
                )
                ( EClause
                    ()
                    ( POr
                        ()
                        ()
                        (PConstructor () (Label () "LessThan") [])
                        (PConstructor () (Label () "EqualTo") [])
                    )
                    (CPlain () [] (ELiteral () (LBool True)) :| [])
                    <| EClause
                      ()
                      (PConstructor () (Label () "GreaterThan") [])
                      (CPlain () [] (ELiteral () (LBool False)) :| [])
                      :| []
                )
            )
        )
    )
