{-# LANGUAGE OverloadedStrings #-}

module Noll.CompilerExamples.Test02 where

import Control.Monad.Identity (runIdentity)
import Control.Monad.State (evalState)
import Data.Data (Data)
import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Compiler
import Noll.Language (
  BinaryOperator (..),
  Choice (..),
  Clause (..),
  Constructor (..),
  Expression (..),
  IndexedType,
  Intrinsic (..),
  Kind (..),
  Parameter (..),
  Pattern (..),
  Primitive (..),
  Scheme (..),
  Trait (..),
  Type (..),
  TypeIndex (..),
  Uses (..),
 )
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..))
import Noll.SystemFSpec.TestRunner
import Test.Hspec (Spec, describe, it)

import qualified Data.Set as Set
import qualified Lang.Common.Environment as Environment
import qualified Noll.Examples.Test03 as Test03
import qualified Noll.Examples.Test04 as Test04

spec :: Spec
spec =
  describe "Noll.Compiler" $ do
    it "" $ do
      testResultExpression (baz fixture) == fixture1
    it "" $ do
      testResultExpression (compileOrPatterns Test03.moduleOrdered) == Test04.moduleOrdered
    it "" $ do
      testResultExpression (baz3 Test03.moduleBinarySearch) == Test04.moduleBinarySearch

tree0 :: IndexedType
tree0 =
  TApplication
    KType
    (TConstructor (KArrow KType KType) "Tree")
    (TVariable (TypeIndex KType 0) :| [])

tvariable0 :: IndexedType
tvariable0 = TVariable (TypeIndex KType 0)

tvariable1 :: IndexedType
tvariable1 = TVariable (TypeIndex KType 1)

bool :: IndexedType
bool = TIntrinsic IBool

baz3 :: (Show a, Eq a, Data a) => Module a Kind t -> TestResult (Module a Kind (Type TypeIndex Kind)) a
baz3 =
  runTypedModuleTest
    ( CompilerEnvironment
        ( Environment.fromList
            [
              ( "Node"
              , Constructor
                  "Node"
                  3
                  ( Forall
                      (Set.fromList [TypeIndex KType 0])
                      []
                      ( tvariable0
                          `TArrow` tree0
                          `TArrow` tree0
                          `TArrow` tree0
                      )
                  )
              )
            ,
              ( "Leaf"
              , Constructor
                  "Leaf"
                  0
                  ( Forall
                      (Set.fromList [TypeIndex KType 0])
                      []
                      tree0
                  )
              )
            ]
        )
        ( Environment.fromList
            [
              ( "Tree"
              , KArrow KType KType
              )
            ]
        )
    )
    []

compileOrPatterns :: (Data a, Show a, Eq a) => Module a Kind t -> TestResult (Module a Kind (Type TypeIndex Kind)) a
compileOrPatterns =
  runTypedModuleTest
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
    ]

baz :: [Definition () Kind t] -> TestResult [Definition () Kind IndexedType] ()
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
    ]

fixture :: [Definition () k ()]
fixture =
  [ DFunction
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
              (EBinaryOperator () () OReverseComposition)
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
  , DFunction
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
  ]

fixture1 :: [Definition () Kind IndexedType]
fixture1 =
  [ DFunction
      "greater_than"
      ( Function
          ()
          (Uses [] (tvariable1 `TArrow` bool))
          ( PAnnotation
              ()
              (TVariable (Parameter () "a"))
              (PVariable () (Label tvariable1 "n"))
              :| []
          )
          ( EApplication
              ()
              (tvariable1 `TArrow` bool)
              (EBinaryOperator () ((bool `TArrow` bool) `TArrow` (tvariable1 `TArrow` bool) `TArrow` tvariable1 `TArrow` bool) OReverseComposition)
              ( EVariable () (Label (bool `TArrow` bool) "not")
                  <| EApplication
                    ()
                    (tvariable1 `TArrow` bool)
                    (EVariable () (Label (tvariable1 `TArrow` tvariable1 `TArrow` bool) "less_than_or_equal_to"))
                    (EVariable () (Label tvariable1 "n") :| [])
                  :| []
              )
          )
      )
  , DFunction
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
  ]
