{-# LANGUAGE OverloadedStrings #-}

module Noll.CompilerExamples.Test01 where

import Control.Monad.State (evalState)
import Control.Monad.Identity (runIdentity)
import Noll.Common.List1 (NonEmpty (..), (<|))
import Noll.Compiler
import Noll.Label (Label (..))
import Noll.Language (
  BinaryOperator (..),
  Choice (..),
  Clause (..),
  Constructor (..),
  Definition (..),
  Expression (..),
  Function (..),
  IndexedType,
  Intrinsic (..),
  Kind (..),
  Module (..),
  Parameter (..),
  Path (..),
  Pattern (..),
  Primitive (..),
  Scheme (..),
  Trait (..),
  TypeIndex (..),
  Type (..),
  Uses (..),
 )
import Test.Hspec (Spec, describe, it)
import Noll.SystemFSpec.TestRunner

import qualified Noll.Common.Environment as Environment

spec :: Spec
spec =
  describe "Noll.Compiler" $ do
    it "" $ do
      1 == 2

baz :: (Show a, Eq a) => TestResult (Expression a (Type TypeIndex Kind)) a
baz =
    runTypedExpressionTest
        (CompilerEnvironment
            ( Environment.fromList
                [
                  ( "LessThan"
                  , Constructor
                      "LessThan"
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
                  ( "EqualTo"
                  , Constructor
                      "EqualTo"
                      0
                      (Forall mempty [] (TConstructor KType "Ordering"))
                  )
                ]
            )
            ( Environment.fromList
                []
            )
          )
          [
          ]
          undefined -- (traverse typeCheckDefinitionC =<< fixture1)

fixture1 :: (Monad m) => CompilerT a m [Definition () () IndexedType]
fixture1 = traverse indexedC fixture

fixture :: [Definition () () ()]
fixture =
  [ ( DFunction
        "less_than_or_equal_to"
        ( Function
            ()
            (Uses [] ())
            (PVariable () (Label () "m") :| [])
            ( ELambda
                ()
                (PVariable () (Label () "n") :| [])
                ( EMatch
                    ()
                    ()
                    ( EApplication
                        ()
                        ()
                        (EVariable () (Label () "compare"))
                        ( EVariable () (Label () "m")
                            <| EVariable () (Label () "n")
                            :| []
                        )
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
    )
  , ( DFunction
        "greater_than"
        ( Function
            ()
            (Uses [] ())
            ( PAnnotation
                ()
                (TVariable (Parameter () "a"))
                (PVariable () (Label () "n"))
                :| []
            )
            ( EApplication
                ()
                ()
                (EBinaryOperator () ((), OReverseComposition))
                ( EVariable () (Label () "not")
                    <| EApplication
                      ()
                      ()
                      (EVariable () (Label () "less_than_or_equal_to"))
                      (EVariable () (Label () "n") :| [])
                    :| []
                )
            )
        )
    )
  ]
