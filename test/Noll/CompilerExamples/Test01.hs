{-# LANGUAGE OverloadedStrings #-}

module Noll.CompilerExamples.Test01 where

import Control.Monad.Identity (runIdentity)
import Control.Monad.State (evalState)
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
  Type (..),
  TypeIndex (..),
  Uses (..),
 )
import Noll.SystemFSpec.TestRunner
import Test.Hspec (Spec, describe, it)

import qualified Data.Set as Set
import qualified Noll.Common.Environment as Environment

spec :: Spec
spec =
  describe "Noll.Compiler" $ do
    it "" $ do
      testResultExpression (baz fixture) == fixture1

-- baz :: TestResult [Definition () Kind IndexedType] ()
baz =
  runTypedDefinitionsTest
    ( CompilerEnvironment
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
      ( "compare"
      , Forall
          (Set.fromList [TypeIndex KType 0])
          []
          ( TVariable (TypeIndex KType 0)
              `TArrow` TVariable (TypeIndex KType 0)
              `TArrow` TConstructor KType "Ordering"
          )
      )
    ,
      ( "not"
      , Forall
          mempty
          []
          (TIntrinsic IBool `TArrow` TIntrinsic IBool)
      )
    ,
      ( "less_than_or_equal_to"
      , Forall
          (Set.fromList [TypeIndex KType 0])
          []
          ( TVariable (TypeIndex KType 0)
              `TArrow` TVariable (TypeIndex KType 0)
              `TArrow` TIntrinsic IBool
          )
      )
    ]

fixture :: [Definition () k ()]
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

tvariable0 :: IndexedType
tvariable0 = TVariable (TypeIndex KType 0)

bool :: IndexedType
bool = TIntrinsic IBool

fixture1 :: [Definition () Kind IndexedType]
fixture1 =
  [ ( DFunction
        "less_than_or_equal_to"
        ( Function
            ()
            (Uses [] (tvariable0 `TArrow` bool))
            (PVariable () (Label tvariable0 "m") :| [])
            ( ELambda
                ()
                (PVariable () (Label tvariable0 "n") :| [])
                ( EMatch
                    ()
                    bool
                    ( EApplication
                        ()
                        (TConstructor KType "Ordering")
                        (EVariable () (Label (tvariable0 `TArrow` tvariable0 `TArrow` TConstructor KType "Ordering") "compare"))
                        ( EVariable () (Label tvariable0 "m")
                            <| EVariable () (Label tvariable0 "n")
                            :| []
                        )
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
    )
  , ( DFunction
        "greater_than"
        ( Function
            ()
            (Uses [] (tvariable0 `TArrow` bool))
            ( PAnnotation
                ()
                (TVariable (Parameter () "a"))
                (PVariable () (Label tvariable0 "n"))
                :| []
            )
            ( EApplication
                ()
                (tvariable0 `TArrow` bool)
                (EBinaryOperator () ((bool `TArrow` bool) `TArrow` (tvariable0 `TArrow` bool) `TArrow` tvariable0 `TArrow` bool, OReverseComposition))
                ( EVariable () (Label (bool `TArrow` bool) "not")
                    <| EApplication
                      ()
                      (tvariable0 `TArrow` bool)
                      (EVariable () (Label (tvariable0 `TArrow` tvariable0 `TArrow` bool) "less_than_or_equal_to"))
                      (EVariable () (Label tvariable0 "n") :| [])
                    :| []
                )
            )
        )
    )
  ]
