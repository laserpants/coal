{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystemExamples.Test09 where

import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Lang.Label (Label (..))
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
  With (..),
 )
import Noll.Language.Module (Constant (..), Definition (..), Function (..), Module (..))
import Noll.TypeSystemSpec.TestRunner
import Test.Hspec (Spec, describe, hspec, it)

import qualified Data.Set as Set
import qualified Lang.Common.Environment as Environment

spec :: Spec
spec =
  describe "lte(x) = fn(y) => match(compare(x, y)) { LessThan or EqualTo => true | GreaterThan => false }" $
    it "" $ do
      testResultExpression (runTest fixture) == fixture1

runTest :: (Show a, Eq a, Data a) => Function Expression a () -> TestResult (Function Expression a (Type TypeIndex Kind)) a
runTest =
  runTypedFunctionTest
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
-- lte(x) =
--   fn(y) =>
--     match(compare(x, y)) {
--       | LessThan or EqualTo => true
--       | GreaterThan => false
--     }
--
fixture :: Function Expression () ()
fixture =
  ( Function
      ()
      (With [] ())
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

--
-- lte(x) =
--   fn(y) =>
--     match(compare(x, y)) {
--       | LessThan or EqualTo => true
--       | GreaterThan => false
--     }
--
fixture1 :: Function Expression () (Type TypeIndex Kind)
fixture1 =
  ( Function
      ()
      (With [] (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool))
      (PVariable () (Label (TVariable (TypeIndex KType 0)) "x") :| [])
      ( ELambda
          ()
          (PVariable () (Label (TVariable (TypeIndex KType 0)) "y") :| [])
          ( EMatch
              ()
              (TIntrinsic IBool)
              ( EApplication
                  ()
                  (TConstructor KType "Ordering")
                  (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TConstructor KType "Ordering") "compare"))
                  (EVariable () (Label (TVariable (TypeIndex KType 0)) "x") <| EVariable () (Label (TVariable (TypeIndex KType 0)) "y") :| [])
              )
              ( EClause
                  ()
                  ( POr
                      ()
                      (TConstructor KType "Ordering")
                      (PConstructor () (Label (TConstructor KType "Ordering") "LessThan") [])
                      (PConstructor () (Label (TConstructor KType "Ordering") "EqualTo") [])
                  )
                  (CPlain () [] (ELiteral () (LBool True)) :| [])
                  <| EClause
                    ()
                    (PConstructor () (Label (TConstructor KType "Ordering") "GreaterThan") [])
                    (CPlain () [] (ELiteral () (LBool False)) :| [])
                    :| []
              )
          )
      )
  )
