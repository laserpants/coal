{-# LANGUAGE OverloadedStrings #-}

module Noll.SystemFExamples.Test01 where

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
import Noll.SystemFSpec.TestRunner
import Test.Hspec (Spec, describe, it)

import qualified Data.Set as Set
import qualified Lang.Common.Environment as Environment

spec :: Spec
spec =
  describe "let lte = fn(x) => fn(y) => match(compare(x, y)) { LessThan or EqualTo => true | GreaterThan => false } in lte" $
    it "" $ do
      testResultExpression (runTest fixture)
        == ( ELet
              ()
              ( BPattern
                  ()
                  (PVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "lte"))
                  ( ELambda
                      ()
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
                  :| []
              )
              (EVariable () (Label (TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool) "lte"))
           )

runTest :: (Show a, Eq a, Data a) => Expression a () -> TestResult (Expression a (Type TypeIndex Kind)) a
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
  ( ELet
      ()
      ( BPattern
          ()
          (PVariable () (Label () "lte"))
          ( ELambda
              ()
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
          :| []
      )
      (EVariable () (Label () "lte"))
  )
