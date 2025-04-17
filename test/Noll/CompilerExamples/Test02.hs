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
  Row (..),
  Scheme (..),
  Trait (..),
  Type (..),
  TypeIndex (..),
  With (..),
 )
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..))
import Noll.SystemFSpec.TestRunner
import Test.Hspec (Spec, describe, it)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Lang.Common.Environment as Environment
import qualified Noll.Examples.Test03 as Test03
import qualified Noll.Examples.Test04 as Test04
import qualified Noll.Set.Test03
import qualified Noll.Set.Test04

spec :: Spec
spec =
  describe "Noll.Compiler" $ do
    --    it "" $ do
    --      testResultExpression (baz fixture) == fixture1
    --    it "" $ do
    --      testResultExpression (compileOrPatterns Test03.moduleOrdered) == Test04.moduleOrdered
    --    it "" $ do
    --      testResultExpression (baz3 Test03.moduleBinarySearch) == Test04.moduleBinarySearch
    --    it "" $ do
    --      testResultExpression (baz3 Noll.Set.Test03.moduleUtils) == Noll.Set.Test04.moduleUtils
    --    it "" $ do
    --      testResultExpression (baz3 Noll.Set.Test03.moduleOrdered) == Noll.Set.Test04.moduleOrdered
    --    it "" $ do
    --      testResultExpression (baz3 Noll.Set.Test03.moduleBinarySearch) == Noll.Set.Test04.moduleBinarySearch
    --    it "" $ do
    --      testResultExpression (baz3 Noll.Set.Test03.moduleMain) == Noll.Set.Test04.moduleMain
    it "" $ do
      testResultExpression (Noll.CompilerExamples.Test02.baz Noll.CompilerExamples.Test02.fixture4) == Noll.CompilerExamples.Test02.fixture41
    it "" $ do
      testResultExpression (Noll.CompilerExamples.Test02.baz Noll.CompilerExamples.Test02.fixture5) == Noll.CompilerExamples.Test02.fixture51

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
            ,
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
      ( "from_int32"
      , Forall
          (Set.fromList [TypeIndex KType 0])
          []
          ( TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0)
          )
      )
    ,
      ( "in_range"
      , Forall
          (Set.fromList [TypeIndex KType 0])
          []
          ( TIntrinsic
              ( IRecord
                  ( TRow
                      ( RExtend
                          "max"
                          (TVariable (TypeIndex KType 0))
                          (RExtend "min" (TVariable (TypeIndex KType 0)) RNil)
                      )
                  )
              )
              `TArrow` TVariable (TypeIndex KType 0)
              `TArrow` TIntrinsic IBool
          )
      )
    ,
      ( "always"
      , Forall
          (Set.fromList [TypeIndex KType 0, TypeIndex KType 1])
          []
          ( TVariable (TypeIndex KType 0)
              `TArrow` TVariable (TypeIndex KType 1)
              `TArrow` TVariable (TypeIndex KType 0)
          )
      )
    ,
      ( "sort"
      , Forall
          (Set.fromList [TypeIndex KType 0])
          []
          ( TIntrinsic (IList (TVariable (TypeIndex KType 0)))
                `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 0))))
      )
    ]

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
            ,
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
      ( "from_int32"
      , Forall
          (Set.fromList [TypeIndex KType 0])
          []
          ( TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0)
          )
      )
    ,
      ( "in_range"
      , Forall
          (Set.fromList [TypeIndex KType 0])
          []
          ( TIntrinsic
              ( IRecord
                  ( TRow
                      ( RExtend
                          "max"
                          (TVariable (TypeIndex KType 0))
                          (RExtend "min" (TVariable (TypeIndex KType 0)) RNil)
                      )
                  )
              )
              `TArrow` TVariable (TypeIndex KType 0)
              `TArrow` TIntrinsic IBool
          )
      )
    ,
      ( "always"
      , Forall
          (Set.fromList [TypeIndex KType 0, TypeIndex KType 1])
          []
          ( TVariable (TypeIndex KType 0)
              `TArrow` TVariable (TypeIndex KType 1)
              `TArrow` TVariable (TypeIndex KType 0)
          )
      )
    ,
      ( "sort"
      , Forall
          (Set.fromList [TypeIndex KType 0])
          []
          ( TIntrinsic (IList (TVariable (TypeIndex KType 0)))
                `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 0))))
      )
      --    ,
      --      ( "baz"
      --      , Forall
      --          (Set.fromList [TypeIndex KType 0])
      --          []
      --          ( TIntrinsic
      --              ( IRecord
      --                  ( TRow
      --                      ( RExtend
      --                          "max"
      --                          (TVariable (TypeIndex KType 0))
      --                          (RExtend "min" (TVariable (TypeIndex KType 0)) RNil)
      --                      )
      --                  )
      --              )
      --              `TArrow` ( TApplication
      --                          KType
      --                          (TConstructor (KArrow KType KType) "Tree")
      --                          (TVariable (TypeIndex KType 0) :| [])
      --                       )
      --          )
      --      )
    ]

baz2 :: Expression () () -> TestResult (Expression () IndexedType) ()
baz2 =
  runTypedExpressionTest
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
            ,
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
      ( "from_int32"
      , Forall
          (Set.fromList [TypeIndex KType 0])
          []
          ( TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0)
          )
      )
    ,
      ( "in_range"
      , Forall
          (Set.fromList [TypeIndex KType 0])
          []
          ( TIntrinsic
              ( IRecord
                  ( TRow
                      ( RExtend
                          "max"
                          (TVariable (TypeIndex KType 0))
                          (RExtend "min" (TVariable (TypeIndex KType 0)) RNil)
                      )
                  )
              )
              `TArrow` TVariable (TypeIndex KType 0)
              `TArrow` TIntrinsic IBool
          )
      )
    ,
      ( "always"
      , Forall
          (Set.fromList [TypeIndex KType 0, TypeIndex KType 1])
          []
          ( TVariable (TypeIndex KType 0)
              `TArrow` TVariable (TypeIndex KType 1)
              `TArrow` TVariable (TypeIndex KType 0)
          )
      )
    ,
      ( "sort"
      , Forall
          (Set.fromList [TypeIndex KType 0])
          []
          ( TIntrinsic (IList (TVariable (TypeIndex KType 0)))
                `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 0))))
      )
      --    ,
      --      ( "baz"
      --      , Forall
      --          (Set.fromList [TypeIndex KType 0])
      --          []
      --          ( TIntrinsic
      --              ( IRecord
      --                  ( TRow
      --                      ( RExtend
      --                          "max"
      --                          (TVariable (TypeIndex KType 0))
      --                          (RExtend "min" (TVariable (TypeIndex KType 0)) RNil)
      --                      )
      --                  )
      --              )
      --              `TArrow` ( TApplication
      --                          KType
      --                          (TConstructor (KArrow KType KType) "Tree")
      --                          (TVariable (TypeIndex KType 0) :| [])
      --                       )
      --          )
      --      )
    ]

fixture :: [Definition () k ()]
fixture =
  [ DFunction
      "greater_than"
      ( Function
          ()
          (With [] ())
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
          (With [] ())
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
          (With [] (tvariable1 `TArrow` bool))
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
          (With [] (tvariable0 `TArrow` bool))
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

-- moduleOrdered
fixture4 =
  [ ( DFunction
        "less_than_or_equal_to"
        ( Function
            ()
            (With [] ())
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
            (With [] ())
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
    )
  ]

fixture41 :: [Definition () Kind IndexedType]
fixture41 =
  [ ( DFunction
        "less_than_or_equal_to"
        ( Function
            ()
            (With [] (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool))
            (PVariable () (Label (TVariable (TypeIndex KType 0)) "m") :| [])
            ( ELambda
                ()
                (PVariable () (Label (TVariable (TypeIndex KType 0)) "n") :| [])
                ( EMatch
                    ()
                    (TIntrinsic IBool)
                    ( EApplication
                        ()
                        (TConstructor KType "Ordering")
                        (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TConstructor KType "Ordering") "compare"))
                        ( EVariable () (Label (TVariable (TypeIndex KType 0)) "m")
                            <| EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
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
            (With [] (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool))
            ( PAnnotation
                ()
                (TVariable (Parameter () "a"))
                (PVariable () (Label (TVariable (TypeIndex KType 1)) "n"))
                :| []
            )
            ( EApplication
                ()
                (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool)
                ( EBinaryOperator
                    ()
                    ( (TIntrinsic IBool `TArrow` TIntrinsic IBool)
                        `TArrow` (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool)
                        `TArrow` TVariable (TypeIndex KType 1)
                        `TArrow` TIntrinsic IBool
                    )
                    OReverseComposition
                )
                ( EVariable () (Label (TIntrinsic IBool `TArrow` TIntrinsic IBool) "not")
                    <| EApplication
                      ()
                      (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool)
                      (EVariable () (Label (TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool) "less_than_or_equal_to"))
                      (EVariable () (Label (TVariable (TypeIndex KType 1)) "n") :| [])
                    :| []
                )
            )
        )
    )
  ]

-- moduleBinarySearch
fixture5 =
  [ DAnnotation
      ( With
          [ Trait "Ordered" (TVariable (Parameter () "a"))
          , Trait "Numeric" (TVariable (Parameter () "a"))
          ]
          (TIntrinsic IBool)
      )
      ( DFunction
          "in_range"
          ( Function
              ()
              (With [] ())
              ( PAnnotation
                  ()
                  ( TAlias
                      "Range"
                      [TVariable (Parameter () "a")]
                      ( TIntrinsic
                          ( IRecord
                              ( TRow
                                  ( RExtend
                                      "max"
                                      (TVariable (Parameter () "a"))
                                      (RExtend "min" (TVariable (Parameter () "a")) RNil)
                                  )
                              )
                          )
                      )
                  )
                  (PVariable () (Label () "range"))
                  <| PAnnotation
                    ()
                    (TVariable (Parameter () "a"))
                    (PVariable () (Label () "n"))
                  :| []
              )
              ( EApplication
                  ()
                  ()
                  (EBinaryOperator () () OLogicalAnd)
                  ( EApplication
                      ()
                      ()
                      (EVariable () (Label () "greater_than"))
                      ( EVariable () (Label () "n")
                          <| ESelect () (Label () "min") (EVariable () (Label () "range"))
                          :| []
                      )
                      <| ( EApplication
                            ()
                            ()
                            (EBinaryOperator () () OLogicalOr)
                         )
                        ( EApplication
                            ()
                            ()
                            (EVariable () (Label () "less_than_or_equal_to"))
                            ( EVariable () (Label () "n")
                                <| ESelect () (Label () "max") (EVariable () (Label () "range"))
                                :| []
                            )
                            <| EApplication
                              ()
                              ()
                              (EBinaryOperator () () OEqualTo)
                              ( ESelect () (Label () "max") (EVariable () (Label () "range"))
                                  <| EApplication
                                    ()
                                    ()
                                    (EVariable () (Label () "from_int32"))
                                    (ELiteral () (LInt32 (-1)) :| [])
                                  :| []
                              )
                            :| []
                        )
                      :| []
                  )
              )
          )
      )
  , ( DFunction
        "from_list"
        ( Function
            ()
            (With [] ())
            (PAnnotation () (TIntrinsic (IList (TVariable (Parameter () "a")))) (PVariable () (Label () "list")) :| [])
            ( EFold
                ()
                ()
                ( EVariable () (Label () "list")
                    <| ERecord
                      ()
                      ()
                      ( Map.fromList
                          [
                            ( "min"
                            , EApplication
                                ()
                                ()
                                (EVariable () (Label () "from_int32"))
                                (ELiteral () (LInt32 0) :| [])
                            )
                          ,
                            ( "max"
                            , EApplication
                                ()
                                ()
                                (EVariable () (Label () "from_int32"))
                                (ELiteral () (LInt32 (-1)) :| [])
                            )
                          ]
                      )
                      Nothing
                    :| []
                )
                ( EClause
                    ()
                    ( PListCons
                        ()
                        ()
                        (PVariable () (Label () "p"))
                        (PAtVariable () (Label () "g"))
                    )
                    ( CPlain
                        ()
                        []
                        ( ELambda
                            ()
                            (PVariable () (Label () "range") :| [])
                            ( EIf
                                ()
                                ()
                                ( EApplication
                                    ()
                                    ()
                                    (EBinaryOperator () () OReverseApplication)
                                    ( EVariable () (Label () "p")
                                        <| EApplication
                                          ()
                                          ()
                                          (EVariable () (Label () "in_range"))
                                          (EVariable () (Label () "range") :| [])
                                        :| []
                                    )
                                )
                                ( EApplication
                                    ()
                                    ()
                                    (EConstructor () (Label () "Node"))
                                    ( EVariable () (Label () "p")
                                        <| EApplication
                                          ()
                                          ()
                                          (EVariable () (Label () "g"))
                                          ( ERecord
                                              ()
                                              ()
                                              ( Map.fromList
                                                  [
                                                    ( "min"
                                                    , ESelect () (Label () "min") (EVariable () (Label () "range"))
                                                    )
                                                  ,
                                                    ( "max"
                                                    , EVariable () (Label () "p")
                                                    )
                                                  ]
                                              )
                                              Nothing
                                              :| []
                                          )
                                        <| EApplication
                                          ()
                                          ()
                                          (EVariable () (Label () "g"))
                                          ( ERecord
                                              ()
                                              ()
                                              ( Map.fromList
                                                  [
                                                    ( "min"
                                                    , EVariable () (Label () "p")
                                                    )
                                                  ,
                                                    ( "max"
                                                    , ESelect () (Label () "max") (EVariable () (Label () "range"))
                                                    )
                                                  ]
                                              )
                                              Nothing
                                              :| []
                                          )
                                        :| []
                                    )
                                )
                                (EApplication () () (EVariable () (Label () "g")) (EVariable () (Label () "range") :| []))
                            )
                        )
                        :| []
                    )
                    <| EClause
                      ()
                      (PListLiteral () () [])
                      ( CPlain
                          ()
                          []
                          ( EApplication
                              ()
                              ()
                              (EVariable () (Label () "always"))
                              (EConstructor () (Label () "Leaf") :| [])
                          )
                          :| []
                      )
                    :| []
                )
                ( Just
                    ( ERecursiveLet
                        ()
                        (PVariable () (Label () "$fold.1"))
                        ( ELambda
                            ()
                            (PVariable () (Label () "$fold.1.expr") :| [])
                            ( EMatch
                                ()
                                ()
                                (EVariable () (Label () "$fold.1.expr"))
                                ( EClause
                                    ()
                                    (PListCons () () (PVariable () (Label () "p")) (PVariable () (Label () "g")))
                                    ( CPlain
                                        ()
                                        []
                                        ( ELambda
                                            ()
                                            (PVariable () (Label () "range") :| [])
                                            ( EIf
                                                ()
                                                ()
                                                ( EApplication
                                                    ()
                                                    ()
                                                    (EBinaryOperator () () OReverseApplication)
                                                    ( EVariable () (Label () "p")
                                                        <| EApplication
                                                          ()
                                                          ()
                                                          (EVariable () (Label () "in_range"))
                                                          (EVariable () (Label () "range") :| [])
                                                        :| []
                                                    )
                                                )
                                                ( EApplication
                                                    ()
                                                    ()
                                                    (EConstructor () (Label () "Node"))
                                                    ( EVariable () (Label () "p")
                                                        <| EApplication
                                                          ()
                                                          ()
                                                          (EVariable () (Label () "$fold.1"))
                                                          ( EVariable () (Label () "g")
                                                              :| [ ERecord
                                                                    ()
                                                                    ()
                                                                    ( Map.fromList
                                                                        [
                                                                          ( "max"
                                                                          , EVariable () (Label () "p")
                                                                          )
                                                                        ,
                                                                          ( "min"
                                                                          , ESelect
                                                                              ()
                                                                              (Label () "min")
                                                                              (EVariable () (Label () "range"))
                                                                          )
                                                                        ]
                                                                    )
                                                                    Nothing
                                                                 ]
                                                          )
                                                        <| EApplication
                                                          ()
                                                          ()
                                                          (EVariable () (Label () "$fold.1"))
                                                          ( EVariable () (Label () "g")
                                                              <| ERecord
                                                                ()
                                                                ()
                                                                ( Map.fromList
                                                                    [
                                                                      ( "max"
                                                                      , ESelect
                                                                          ()
                                                                          (Label () "max")
                                                                          (EVariable () (Label () "range"))
                                                                      )
                                                                    ,
                                                                      ( "min"
                                                                      , EVariable () (Label () "p")
                                                                      )
                                                                    ]
                                                                )
                                                                Nothing
                                                              :| []
                                                          )
                                                        :| []
                                                    )
                                                )
                                                ( EApplication
                                                    ()
                                                    ()
                                                    (EVariable () (Label () "$fold.1"))
                                                    ( EVariable () (Label () "g")
                                                        <| EVariable () (Label () "range")
                                                        :| []
                                                    )
                                                )
                                            )
                                        )
                                        :| []
                                    )
                                    <| EClause
                                      ()
                                      (PListLiteral () () [])
                                      ( CPlain
                                          ()
                                          []
                                          ( EApplication
                                              ()
                                              ()
                                              (EVariable () (Label () "always"))
                                              ( EConstructor () (Label () "Leaf")
                                                  :| []
                                              )
                                          )
                                          :| []
                                      )
                                    :| []
                                )
                            )
                        )
                        ( EApplication
                            ()
                            ()
                            (EVariable () (Label () "$fold.1"))
                            ( EVariable () (Label () "list")
                                <| ERecord
                                  ()
                                  ()
                                  ( Map.fromList
                                      [
                                        ( "max"
                                        , EApplication
                                            ()
                                            ()
                                            (EVariable () (Label () "from_int32"))
                                            (ELiteral () (LInt32 (-1)) :| [])
                                        )
                                      ,
                                        ( "min"
                                        , EApplication
                                            ()
                                            ()
                                            (EVariable () (Label () "from_int32"))
                                            (ELiteral () (LInt32 0) :| [])
                                        )
                                      ]
                                  )
                                  Nothing
                                :| []
                            )
                        )
                    )
                )
            )
        )
    )
  , ( DFunction
        "flatten"
        ( Function
            ()
            (With [] ())
            ( PAnnotation
                ()
                (TApplication () (TConstructor () "Tree") (TVariable (Parameter () "a") :| []))
                (PVariable () (Label () "tree"))
                :| []
            )
            ( EFold
                ()
                ()
                (EVariable () (Label () "tree") :| [])
                ( EClause
                    ()
                    ( PConstructor
                        ()
                        (Label () "Node")
                        [ PVariable () (Label () "y")
                        , PAtVariable () (Label () "lhs")
                        , PAtVariable () (Label () "rhs")
                        ]
                    )
                    ( CPlain
                        ()
                        []
                        ( EApplication
                            ()
                            ()
                            (EBinaryOperator () () OListConcatenation)
                            ( EVariable () (Label () "lhs")
                                <| EListCons () () (EVariable () (Label () "y")) (EVariable () (Label () "rhs"))
                                :| []
                            )
                        )
                        :| []
                    )
                    <| EClause
                      ()
                      (PConstructor () (Label () "Leaf") [])
                      ( CPlain
                          ()
                          []
                          (EListLiteral () () [])
                          :| []
                      )
                    :| []
                )
                ( Just
                    ( ERecursiveLet
                        ()
                        (PVariable () (Label () "$fold.2"))
                        ( ELambda
                            ()
                            (PVariable () (Label () "$fold.2.expr") :| [])
                            ( EMatch
                                ()
                                ()
                                (EVariable () (Label () "$fold.2.expr"))
                                ( EClause
                                    ()
                                    ( PConstructor
                                        ()
                                        (Label () "Node")
                                        [ PVariable () (Label () "y")
                                        , PVariable () (Label () "lhs")
                                        , PVariable () (Label () "rhs")
                                        ]
                                    )
                                    ( CPlain
                                        ()
                                        []
                                        ( EApplication
                                            ()
                                            ()
                                            (EBinaryOperator () () OListConcatenation)
                                            ( EApplication
                                                ()
                                                ()
                                                (EVariable () (Label () "$fold.2"))
                                                ( EVariable () (Label () "lhs") :| []
                                                )
                                                <| EListCons
                                                  ()
                                                  ()
                                                  (EVariable () (Label () "y"))
                                                  ( EApplication
                                                      ()
                                                      ()
                                                      (EVariable () (Label () "$fold.2"))
                                                      ( EVariable () (Label () "rhs")
                                                          :| []
                                                      )
                                                  )
                                                :| []
                                            )
                                        )
                                        :| []
                                    )
                                    <| EClause
                                      ()
                                      (PConstructor () (Label () "Leaf") [])
                                      (CPlain () [] (EListLiteral () () []) :| [])
                                    :| []
                                )
                            )
                        )
                        ( EApplication
                            ()
                            ()
                            (EVariable () (Label () "$fold.2"))
                            (EVariable () (Label () "tree") :| [])
                        )
                    )
                )
            )
        )
    )
  , ( DConstant
        "sort"
        ( Constant
            ()
            (With [] ())
            ( EApplication
                ()
                ()
                (EBinaryOperator () () OReverseComposition)
                ( EVariable () (Label () "flatten")
                    <| EVariable () (Label () "from_list")
                    :| []
                )
            )
        )
    )
  ]

fixture51 :: [Definition () Kind IndexedType]
fixture51 =
  [ DAnnotation
      ( With
          [ Trait "Ordered" (TVariable (Parameter () "a"))
          , Trait "Numeric" (TVariable (Parameter () "a"))
          ]
          (TIntrinsic IBool)
      )
      ( DFunction
          "in_range"
          ( Function
              ()
              (With [] (TIntrinsic IBool))
              ( PAnnotation
                  ()
                  ( TAlias
                      "Range"
                      [TVariable (Parameter () "a")]
                      ( TIntrinsic
                          ( IRecord
                              ( TRow
                                  ( RExtend
                                      "max"
                                      (TVariable (Parameter () "a"))
                                      (RExtend "min" (TVariable (Parameter () "a")) RNil)
                                  )
                              )
                          )
                      )
                  )
                  ( PVariable
                      ()
                      ( Label
                          ( TIntrinsic
                              ( IRecord
                                  ( TRow
                                      ( RExtend
                                          "max"
                                          (TVariable (TypeIndex KType 0))
                                          ( RExtend
                                              "min"
                                              (TVariable (TypeIndex KType 0))
                                              RNil
                                          )
                                      )
                                  )
                              )
                          )
                          "range"
                      )
                  )
                  <| PAnnotation
                    ()
                    (TVariable (Parameter () "a"))
                    (PVariable () (Label (TVariable (TypeIndex KType 0)) "n"))
                  :| []
              )
              ( EApplication
                  ()
                  (TIntrinsic IBool)
                  (EBinaryOperator () (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) OLogicalAnd)
                  ( EApplication
                      ()
                      (TIntrinsic IBool)
                      (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "greater_than"))
                      ( EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                          <| ESelect
                            ()
                            (Label (TVariable (TypeIndex KType 0)) "min")
                            ( EVariable
                                ()
                                ( Label
                                    ( TIntrinsic
                                        ( IRecord
                                            ( TRow
                                                ( RExtend
                                                    "max"
                                                    (TVariable (TypeIndex KType 0))
                                                    ( RExtend
                                                        "min"
                                                        (TVariable (TypeIndex KType 0))
                                                        RNil
                                                    )
                                                )
                                            )
                                        )
                                    )
                                    "range"
                                )
                            )
                          :| []
                      )
                      <| ( EApplication
                            ()
                            (TIntrinsic IBool)
                            (EBinaryOperator () (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) OLogicalOr)
                         )
                        ( EApplication
                            ()
                            (TIntrinsic IBool)
                            (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "less_than_or_equal_to"))
                            ( EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                                <| ESelect
                                  ()
                                  (Label (TVariable (TypeIndex KType 0)) "max")
                                  ( EVariable
                                      ()
                                      ( Label
                                          ( TIntrinsic
                                              ( IRecord
                                                  ( TRow
                                                      ( RExtend
                                                          "max"
                                                          (TVariable (TypeIndex KType 0))
                                                          ( RExtend
                                                              "min"
                                                              (TVariable (TypeIndex KType 0))
                                                              RNil
                                                          )
                                                      )
                                                  )
                                              )
                                          )
                                          "range"
                                      )
                                  )
                                :| []
                            )
                            <| EApplication
                              ()
                              (TIntrinsic IBool)
                              ( EBinaryOperator
                                  ()
                                  (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
                                  OEqualTo
                              )
                              ( ESelect
                                  ()
                                  (Label (TVariable (TypeIndex KType 0)) "max")
                                  ( EVariable
                                      ()
                                      ( Label
                                          ( TIntrinsic
                                              ( IRecord
                                                  ( TRow
                                                      ( RExtend
                                                          "max"
                                                          (TVariable (TypeIndex KType 0))
                                                          ( RExtend
                                                              "min"
                                                              (TVariable (TypeIndex KType 0))
                                                              RNil
                                                          )
                                                      )
                                                  )
                                              )
                                          )
                                          "range"
                                      )
                                  )
                                  <| EApplication
                                    ()
                                    (TVariable (TypeIndex KType 0))
                                    (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0)) "from_int32"))
                                    (ELiteral () (LInt32 (-1)) :| [])
                                  :| []
                              )
                            :| []
                        )
                      :| []
                  )
              )
          )
      )
  , ( DFunction
        "from_list"
        ( Function
            ()
            (With [] (TApplication KType (TConstructor (KArrow KType KType) "Tree") (TVariable (TypeIndex KType 2) :| [])))
            ( PAnnotation
                ()
                (TIntrinsic (IList (TVariable (Parameter () "a"))))
                (PVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 2)))) "list"))
                :| []
            )
            ( EFold
                ()
                ( TApplication
                    KType
                    (TConstructor (KArrow KType KType) "Tree")
                    (TVariable (TypeIndex KType 2) :| [])
                )
                ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 2)))) "list")
                    <| ERecord
                      ()
                      ( TIntrinsic
                          ( IRecord
                              ( TRow
                                  ( RExtend
                                      "max"
                                      (TVariable (TypeIndex KType 2))
                                      (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                  )
                              )
                          )
                      )
                      ( Map.fromList
                          [
                            ( "min"
                            , EApplication
                                ()
                                (TVariable (TypeIndex KType 2))
                                (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
                                (ELiteral () (LInt32 0) :| [])
                            )
                          ,
                            ( "max"
                            , EApplication
                                ()
                                (TVariable (TypeIndex KType 2))
                                (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
                                (ELiteral () (LInt32 (-1)) :| [])
                            )
                          ]
                      )
                      Nothing
                    :| []
                )
                ( EClause
                    ()
                    ( PListCons
                        ()
                        (TIntrinsic (IList (TVariable (TypeIndex KType 2))))
                        (PVariable () (Label (TVariable (TypeIndex KType 2)) "p"))
                        (PAtVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 2)))) "g"))
                    )
                    ( CPlain
                        ()
                        []
                        ( ELambda
                            ()
                            ( PVariable
                                ()
                                ( Label
                                    ( TIntrinsic
                                        ( IRecord
                                            ( TRow
                                                ( RExtend
                                                    "max"
                                                    (TVariable (TypeIndex KType 2))
                                                    (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                )
                                            )
                                        )
                                    )
                                    "range"
                                )
                                :| []
                            )
                            ( EIf
                                ()
                                ( TApplication
                                    KType
                                    (TConstructor (KArrow KType KType) "Tree")
                                    (TVariable (TypeIndex KType 2) :| [])
                                )
                                ( EApplication
                                    ()
                                    (TIntrinsic IBool)
                                    ( EBinaryOperator
                                        ()
                                        ( TArrow
                                            (TVariable (TypeIndex KType 2))
                                            (TArrow (TArrow (TVariable (TypeIndex KType 2)) (TIntrinsic IBool)) (TIntrinsic IBool))
                                        )
                                        OReverseApplication
                                    )
                                    ( EVariable () (Label (TVariable (TypeIndex KType 2)) "p")
                                        <| EApplication
                                          ()
                                          (TVariable (TypeIndex KType 2) `TArrow` TIntrinsic IBool)
                                          ( EVariable
                                              ()
                                              ( Label
                                                  ( ( TIntrinsic
                                                        ( IRecord
                                                            ( TRow
                                                                ( RExtend
                                                                    "max"
                                                                    (TVariable (TypeIndex KType 2))
                                                                    (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                                )
                                                            )
                                                        )
                                                    )
                                                      `TArrow` TVariable (TypeIndex KType 2)
                                                      `TArrow` TIntrinsic IBool
                                                  )
                                                  "in_range"
                                              )
                                          )
                                          ( EVariable
                                              ()
                                              ( Label
                                                  ( TIntrinsic
                                                      ( IRecord
                                                          ( TRow
                                                              ( RExtend
                                                                  "max"
                                                                  (TVariable (TypeIndex KType 2))
                                                                  (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                              )
                                                          )
                                                      )
                                                  )
                                                  "range"
                                              )
                                              :| []
                                          )
                                        :| []
                                    )
                                )
                                ( EApplication
                                    ()
                                    ( TApplication
                                        KType
                                        (TConstructor (KArrow KType KType) "Tree")
                                        (TVariable (TypeIndex KType 2) :| [])
                                    )
                                    ( EConstructor
                                        ()
                                        ( Label
                                            ( (TVariable (TypeIndex KType 2))
                                                `TArrow` ( TApplication
                                                            KType
                                                            (TConstructor (KArrow KType KType) "Tree")
                                                            (TVariable (TypeIndex KType 2) :| [])
                                                         )
                                                `TArrow` ( TApplication
                                                            KType
                                                            (TConstructor (KArrow KType KType) "Tree")
                                                            (TVariable (TypeIndex KType 2) :| [])
                                                         )
                                                `TArrow` ( TApplication
                                                            KType
                                                            (TConstructor (KArrow KType KType) "Tree")
                                                            (TVariable (TypeIndex KType 2) :| [])
                                                         )
                                            )
                                            "Node"
                                        )
                                    )
                                    ( EVariable () (Label (TVariable (TypeIndex KType 2)) "p")
                                        <| EApplication
                                          ()
                                          ( TApplication
                                              KType
                                              (TConstructor (KArrow KType KType) "Tree")
                                              (TVariable (TypeIndex KType 2) :| [])
                                          )
                                          ( EVariable
                                              ()
                                              ( Label
                                                  ( ( TIntrinsic
                                                        ( IRecord
                                                            ( TRow
                                                                ( RExtend
                                                                    "max"
                                                                    (TVariable (TypeIndex KType 2))
                                                                    (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                                )
                                                            )
                                                        )
                                                    )
                                                      `TArrow` ( TApplication
                                                                  KType
                                                                  (TConstructor (KArrow KType KType) "Tree")
                                                                  (TVariable (TypeIndex KType 2) :| [])
                                                               )
                                                  )
                                                  "g"
                                              )
                                          )
                                          ( ERecord
                                              ()
                                              ( TIntrinsic
                                                  ( IRecord
                                                      ( TRow
                                                          ( RExtend
                                                              "max"
                                                              (TVariable (TypeIndex KType 2))
                                                              (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                          )
                                                      )
                                                  )
                                              )
                                              ( Map.fromList
                                                  [
                                                    ( "min"
                                                    , ESelect
                                                        ()
                                                        (Label (TVariable (TypeIndex KType 2)) "min")
                                                        ( EVariable
                                                            ()
                                                            ( Label
                                                                ( TIntrinsic
                                                                    ( IRecord
                                                                        ( TRow
                                                                            ( RExtend
                                                                                "max"
                                                                                (TVariable (TypeIndex KType 2))
                                                                                (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                                "range"
                                                            )
                                                        )
                                                    )
                                                  ,
                                                    ( "max"
                                                    , EVariable () (Label (TVariable (TypeIndex KType 2)) "p")
                                                    )
                                                  ]
                                              )
                                              Nothing
                                              :| []
                                          )
                                        <| EApplication
                                          ()
                                          ( TApplication
                                              KType
                                              (TConstructor (KArrow KType KType) "Tree")
                                              (TVariable (TypeIndex KType 2) :| [])
                                          )
                                          ( EVariable
                                              ()
                                              ( Label
                                                  ( ( TIntrinsic
                                                        ( IRecord
                                                            ( TRow
                                                                ( RExtend
                                                                    "max"
                                                                    (TVariable (TypeIndex KType 2))
                                                                    (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                                )
                                                            )
                                                        )
                                                    )
                                                      `TArrow` ( TApplication
                                                                  KType
                                                                  (TConstructor (KArrow KType KType) "Tree")
                                                                  (TVariable (TypeIndex KType 2) :| [])
                                                               )
                                                  )
                                                  "g"
                                              )
                                          )
                                          ( ERecord
                                              ()
                                              ( TIntrinsic
                                                  ( IRecord
                                                      ( TRow
                                                          ( RExtend
                                                              "max"
                                                              (TVariable (TypeIndex KType 2))
                                                              (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                          )
                                                      )
                                                  )
                                              )
                                              ( Map.fromList
                                                  [
                                                    ( "min"
                                                    , EVariable () (Label (TVariable (TypeIndex KType 2)) "p")
                                                    )
                                                  ,
                                                    ( "max"
                                                    , ESelect
                                                        ()
                                                        (Label (TVariable (TypeIndex KType 2)) "max")
                                                        ( EVariable
                                                            ()
                                                            ( Label
                                                                ( TIntrinsic
                                                                    ( IRecord
                                                                        ( TRow
                                                                            ( RExtend
                                                                                "max"
                                                                                (TVariable (TypeIndex KType 2))
                                                                                (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                                "range"
                                                            )
                                                        )
                                                    )
                                                  ]
                                              )
                                              Nothing
                                              :| []
                                          )
                                        :| []
                                    )
                                )
                                ( EApplication
                                    ()
                                    ( TApplication
                                        KType
                                        (TConstructor (KArrow KType KType) "Tree")
                                        (TVariable (TypeIndex KType 2) :| [])
                                    )
                                    ( EVariable
                                        ()
                                        ( Label
                                            ( ( TIntrinsic
                                                  ( IRecord
                                                      ( TRow
                                                          ( RExtend
                                                              "max"
                                                              (TVariable (TypeIndex KType 2))
                                                              (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                          )
                                                      )
                                                  )
                                              )
                                                `TArrow` ( TApplication
                                                            KType
                                                            (TConstructor (KArrow KType KType) "Tree")
                                                            (TVariable (TypeIndex KType 2) :| [])
                                                         )
                                            )
                                            "g"
                                        )
                                    )
                                    ( EVariable
                                        ()
                                        ( Label
                                            ( TIntrinsic
                                                ( IRecord
                                                    ( TRow
                                                        ( RExtend
                                                            "max"
                                                            (TVariable (TypeIndex KType 2))
                                                            (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                        )
                                                    )
                                                )
                                            )
                                            "range"
                                        )
                                        :| []
                                    )
                                )
                            )
                        )
                        :| []
                    )
                    <| EClause
                      ()
                      (PListLiteral () (TIntrinsic (IList (TVariable (TypeIndex KType 2)))) [])
                      ( CPlain
                          ()
                          []
                          ( EApplication
                              ()
                              ( ( TIntrinsic
                                    ( IRecord
                                        ( TRow
                                            ( RExtend
                                                "max"
                                                (TVariable (TypeIndex KType 2))
                                                (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                            )
                                        )
                                    )
                                )
                                  `TArrow` ( TApplication
                                              KType
                                              (TConstructor (KArrow KType KType) "Tree")
                                              (TVariable (TypeIndex KType 2) :| [])
                                           )
                              )
                              ( EVariable
                                  ()
                                  ( Label
                                      ( ( TApplication
                                            KType
                                            (TConstructor (KArrow KType KType) "Tree")
                                            (TVariable (TypeIndex KType 2) :| [])
                                        )
                                          `TArrow` TIntrinsic
                                            ( IRecord
                                                ( TRow
                                                    ( RExtend
                                                        "max"
                                                        (TVariable (TypeIndex KType 2))
                                                        (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                    )
                                                )
                                            )
                                          `TArrow` ( TApplication
                                                      KType
                                                      (TConstructor (KArrow KType KType) "Tree")
                                                      (TVariable (TypeIndex KType 2) :| [])
                                                   )
                                      )
                                      "always"
                                  )
                              )
                              ( EConstructor
                                  ()
                                  ( Label
                                      ( TApplication
                                          KType
                                          (TConstructor (KArrow KType KType) "Tree")
                                          (TVariable (TypeIndex KType 2) :| [])
                                      )
                                      "Leaf"
                                  )
                                  :| []
                              )
                          )
                          :| []
                      )
                    :| []
                )
                ( Just
                    ( ERecursiveLet
                        ()
                        ( PVariable
                            ()
                            ( Label
                                ( TIntrinsic (IList (TVariable (TypeIndex KType 1)))
                                    `TArrow` ( TIntrinsic
                                                ( IRecord
                                                    ( TRow
                                                        ( RExtend
                                                            "max"
                                                            (TVariable (TypeIndex KType 1))
                                                            (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                        )
                                                    )
                                                )
                                             )
                                    `TArrow` ( TApplication
                                                KType
                                                (TConstructor (KArrow KType KType) "Tree")
                                                (TVariable (TypeIndex KType 1) :| [])
                                             )
                                )
                                "$fold.1"
                            )
                        )
                        ( ELambda
                            ()
                            (PVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "$fold.1.expr") :| [])
                            ( EMatch
                                ()
                                ( ( TIntrinsic
                                      ( IRecord
                                          ( TRow
                                              ( RExtend
                                                  "max"
                                                  (TVariable (TypeIndex KType 1))
                                                  (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                              )
                                          )
                                      )
                                  )
                                    `TArrow` ( TApplication
                                                KType
                                                (TConstructor (KArrow KType KType) "Tree")
                                                (TVariable (TypeIndex KType 1) :| [])
                                             )
                                )
                                (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "$fold.1.expr"))
                                ( EClause
                                    ()
                                    ( PListCons
                                        ()
                                        (TIntrinsic (IList (TVariable (TypeIndex KType 1))))
                                        (PVariable () (Label (TVariable (TypeIndex KType 1)) "p"))
                                        (PVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "g"))
                                    )
                                    ( CPlain
                                        ()
                                        []
                                        ( ELambda
                                            ()
                                            ( PVariable
                                                ()
                                                ( Label
                                                    ( TIntrinsic
                                                        ( IRecord
                                                            ( TRow
                                                                ( RExtend
                                                                    "max"
                                                                    (TVariable (TypeIndex KType 1))
                                                                    (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                                )
                                                            )
                                                        )
                                                    )
                                                    "range"
                                                )
                                                :| []
                                            )
                                            ( EIf
                                                ()
                                                ( TApplication
                                                    KType
                                                    (TConstructor (KArrow KType KType) "Tree")
                                                    (TVariable (TypeIndex KType 1) :| [])
                                                )
                                                ( EApplication
                                                    ()
                                                    (TIntrinsic IBool)
                                                    ( EBinaryOperator
                                                        ()
                                                        ( TArrow
                                                            (TVariable (TypeIndex KType 1))
                                                            (TArrow (TArrow (TVariable (TypeIndex KType 1)) (TIntrinsic IBool)) (TIntrinsic IBool))
                                                        )
                                                        OReverseApplication
                                                    )
                                                    ( EVariable () (Label (TVariable (TypeIndex KType 1)) "p")
                                                        <| EApplication
                                                          ()
                                                          (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool)
                                                          ( EVariable
                                                              ()
                                                              ( Label
                                                                  ( ( TIntrinsic
                                                                        ( IRecord
                                                                            ( TRow
                                                                                ( RExtend
                                                                                    "max"
                                                                                    (TVariable (TypeIndex KType 1))
                                                                                    (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                                                )
                                                                            )
                                                                        )
                                                                    )
                                                                      `TArrow` (TVariable (TypeIndex KType 1))
                                                                      `TArrow` (TIntrinsic IBool)
                                                                  )
                                                                  "in_range"
                                                              )
                                                          )
                                                          ( EVariable
                                                              ()
                                                              ( Label
                                                                  ( TIntrinsic
                                                                      ( IRecord
                                                                          ( TRow
                                                                              ( RExtend
                                                                                  "max"
                                                                                  (TVariable (TypeIndex KType 1))
                                                                                  (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                                              )
                                                                          )
                                                                      )
                                                                  )
                                                                  "range"
                                                              )
                                                              :| []
                                                          )
                                                        :| []
                                                    )
                                                )
                                                ( EApplication
                                                    ()
                                                    ( TApplication
                                                        KType
                                                        (TConstructor (KArrow KType KType) "Tree")
                                                        (TVariable (TypeIndex KType 1) :| [])
                                                    )
                                                    ( EConstructor
                                                        ()
                                                        ( Label
                                                            ( (TVariable (TypeIndex KType 1))
                                                                `TArrow` ( TApplication
                                                                            KType
                                                                            (TConstructor (KArrow KType KType) "Tree")
                                                                            (TVariable (TypeIndex KType 1) :| [])
                                                                         )
                                                                `TArrow` ( TApplication
                                                                            KType
                                                                            (TConstructor (KArrow KType KType) "Tree")
                                                                            (TVariable (TypeIndex KType 1) :| [])
                                                                         )
                                                                `TArrow` ( TApplication
                                                                            KType
                                                                            (TConstructor (KArrow KType KType) "Tree")
                                                                            (TVariable (TypeIndex KType 1) :| [])
                                                                         )
                                                            )
                                                            "Node"
                                                        )
                                                    )
                                                    ( EVariable () (Label (TVariable (TypeIndex KType 1)) "p")
                                                        <| EApplication
                                                          ()
                                                          ( TApplication
                                                              KType
                                                              (TConstructor (KArrow KType KType) "Tree")
                                                              (TVariable (TypeIndex KType 1) :| [])
                                                          )
                                                          ( EVariable
                                                              ()
                                                              ( Label
                                                                  ( TIntrinsic (IList (TVariable (TypeIndex KType 1)))
                                                                      `TArrow` ( TIntrinsic
                                                                                  ( IRecord
                                                                                      ( TRow
                                                                                          ( RExtend
                                                                                              "max"
                                                                                              (TVariable (TypeIndex KType 1))
                                                                                              (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                                                          )
                                                                                      )
                                                                                  )
                                                                               )
                                                                      `TArrow` ( TApplication
                                                                                  KType
                                                                                  (TConstructor (KArrow KType KType) "Tree")
                                                                                  (TVariable (TypeIndex KType 1) :| [])
                                                                               )
                                                                  )
                                                                  "$fold.1"
                                                              )
                                                          )
                                                          ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "g")
                                                              :| [ ERecord
                                                                    ()
                                                                    ( TIntrinsic
                                                                        ( IRecord
                                                                            ( TRow
                                                                                ( RExtend
                                                                                    "max"
                                                                                    (TVariable (TypeIndex KType 1))
                                                                                    (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                                                )
                                                                            )
                                                                        )
                                                                    )
                                                                    ( Map.fromList
                                                                        [
                                                                          ( "max"
                                                                          , EVariable () (Label (TVariable (TypeIndex KType 1)) "p")
                                                                          )
                                                                        ,
                                                                          ( "min"
                                                                          , ESelect
                                                                              ()
                                                                              (Label (TVariable (TypeIndex KType 1)) "min")
                                                                              ( EVariable
                                                                                  ()
                                                                                  ( Label
                                                                                      ( TIntrinsic
                                                                                          ( IRecord
                                                                                              ( TRow
                                                                                                  ( RExtend
                                                                                                      "max"
                                                                                                      (TVariable (TypeIndex KType 1))
                                                                                                      (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                                                                  )
                                                                                              )
                                                                                          )
                                                                                      )
                                                                                      "range"
                                                                                  )
                                                                              )
                                                                          )
                                                                        ]
                                                                    )
                                                                    Nothing
                                                                 ]
                                                          )
                                                        <| EApplication
                                                          ()
                                                          ( TApplication
                                                              KType
                                                              (TConstructor (KArrow KType KType) "Tree")
                                                              (TVariable (TypeIndex KType 1) :| [])
                                                          )
                                                          ( EVariable
                                                              ()
                                                              ( Label
                                                                  ( TIntrinsic (IList (TVariable (TypeIndex KType 1)))
                                                                      `TArrow` ( TIntrinsic
                                                                                  ( IRecord
                                                                                      ( TRow
                                                                                          ( RExtend
                                                                                              "max"
                                                                                              (TVariable (TypeIndex KType 1))
                                                                                              (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                                                          )
                                                                                      )
                                                                                  )
                                                                               )
                                                                      `TArrow` ( TApplication
                                                                                  KType
                                                                                  (TConstructor (KArrow KType KType) "Tree")
                                                                                  (TVariable (TypeIndex KType 1) :| [])
                                                                               )
                                                                  )
                                                                  "$fold.1"
                                                              )
                                                          )
                                                          ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "g")
                                                              <| ERecord
                                                                ()
                                                                ( TIntrinsic
                                                                    ( IRecord
                                                                        ( TRow
                                                                            ( RExtend
                                                                                "max"
                                                                                (TVariable (TypeIndex KType 1))
                                                                                (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                                ( Map.fromList
                                                                    [
                                                                      ( "max"
                                                                      , ESelect
                                                                          ()
                                                                          (Label (TVariable (TypeIndex KType 1)) "max")
                                                                          ( EVariable
                                                                              ()
                                                                              ( Label
                                                                                  ( TIntrinsic
                                                                                      ( IRecord
                                                                                          ( TRow
                                                                                              ( RExtend
                                                                                                  "max"
                                                                                                  (TVariable (TypeIndex KType 1))
                                                                                                  (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                                                              )
                                                                                          )
                                                                                      )
                                                                                  )
                                                                                  "range"
                                                                              )
                                                                          )
                                                                      )
                                                                    ,
                                                                      ( "min"
                                                                      , EVariable () (Label (TVariable (TypeIndex KType 1)) "p")
                                                                      )
                                                                    ]
                                                                )
                                                                Nothing
                                                              :| []
                                                          )
                                                        :| []
                                                    )
                                                )
                                                ( EApplication
                                                    ()
                                                    ( TApplication
                                                        KType
                                                        (TConstructor (KArrow KType KType) "Tree")
                                                        (TVariable (TypeIndex KType 1) :| [])
                                                    )
                                                    ( EVariable
                                                        ()
                                                        ( Label
                                                            ( TIntrinsic (IList (TVariable (TypeIndex KType 1)))
                                                                `TArrow` ( TIntrinsic
                                                                            ( IRecord
                                                                                ( TRow
                                                                                    ( RExtend
                                                                                        "max"
                                                                                        (TVariable (TypeIndex KType 1))
                                                                                        (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                                                    )
                                                                                )
                                                                            )
                                                                         )
                                                                `TArrow` ( TApplication
                                                                            KType
                                                                            (TConstructor (KArrow KType KType) "Tree")
                                                                            (TVariable (TypeIndex KType 1) :| [])
                                                                         )
                                                            )
                                                            "$fold.1"
                                                        )
                                                    )
                                                    ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "g")
                                                        <| EVariable
                                                          ()
                                                          ( Label
                                                              ( TIntrinsic
                                                                  ( IRecord
                                                                      ( TRow
                                                                          ( RExtend
                                                                              "max"
                                                                              (TVariable (TypeIndex KType 1))
                                                                              (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                                          )
                                                                      )
                                                                  )
                                                              )
                                                              "range"
                                                          )
                                                        :| []
                                                    )
                                                )
                                            )
                                        )
                                        :| []
                                    )
                                    <| EClause
                                      ()
                                      (PListLiteral () (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) [])
                                      ( CPlain
                                          ()
                                          []
                                          ( EApplication
                                              ()
                                              ( ( TIntrinsic
                                                    ( IRecord
                                                        ( TRow
                                                            ( RExtend
                                                                "max"
                                                                (TVariable (TypeIndex KType 1))
                                                                (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                            )
                                                        )
                                                    )
                                                )
                                                  `TArrow` ( TApplication
                                                              KType
                                                              (TConstructor (KArrow KType KType) "Tree")
                                                              (TVariable (TypeIndex KType 1) :| [])
                                                           )
                                              )
                                              ( EVariable
                                                  ()
                                                  ( Label
                                                      ( ( TApplication
                                                            KType
                                                            (TConstructor (KArrow KType KType) "Tree")
                                                            (TVariable (TypeIndex KType 1) :| [])
                                                        )
                                                          `TArrow` TIntrinsic
                                                            ( IRecord
                                                                ( TRow
                                                                    ( RExtend
                                                                        "max"
                                                                        (TVariable (TypeIndex KType 1))
                                                                        (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                                    )
                                                                )
                                                            )
                                                          `TArrow` ( TApplication
                                                                      KType
                                                                      (TConstructor (KArrow KType KType) "Tree")
                                                                      (TVariable (TypeIndex KType 1) :| [])
                                                                   )
                                                      )
                                                      "always"
                                                  )
                                              )
                                              ( EConstructor
                                                  ()
                                                  ( Label
                                                      ( TApplication
                                                          KType
                                                          (TConstructor (KArrow KType KType) "Tree")
                                                          (TVariable (TypeIndex KType 1) :| [])
                                                      )
                                                      "Leaf"
                                                  )
                                                  :| []
                                              )
                                          )
                                          :| []
                                      )
                                    :| []
                                )
                            )
                        )
                        ( EApplication
                            ()
                            ( TApplication
                                KType
                                (TConstructor (KArrow KType KType) "Tree")
                                (TVariable (TypeIndex KType 2) :| [])
                            )
                            ( EVariable
                                ()
                                ( Label
                                    ( TIntrinsic (IList (TVariable (TypeIndex KType 2)))
                                        `TArrow` ( TIntrinsic
                                                    ( IRecord
                                                        ( TRow
                                                            ( RExtend
                                                                "max"
                                                                (TVariable (TypeIndex KType 2))
                                                                (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                            )
                                                        )
                                                    )
                                                 )
                                        `TArrow` ( TApplication
                                                    KType
                                                    (TConstructor (KArrow KType KType) "Tree")
                                                    (TVariable (TypeIndex KType 2) :| [])
                                                 )
                                    )
                                    "$fold.1"
                                )
                            )
                            ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 2)))) "list")
                                <| ERecord
                                  ()
                                  ( TIntrinsic
                                      ( IRecord
                                          ( TRow
                                              ( RExtend
                                                  "max"
                                                  (TVariable (TypeIndex KType 2))
                                                  (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                              )
                                          )
                                      )
                                  )
                                  ( Map.fromList
                                      [
                                        ( "max"
                                        , EApplication
                                            ()
                                            (TVariable (TypeIndex KType 2))
                                            (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
                                            (ELiteral () (LInt32 (-1)) :| [])
                                        )
                                      ,
                                        ( "min"
                                        , EApplication
                                            ()
                                            (TVariable (TypeIndex KType 2))
                                            (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
                                            (ELiteral () (LInt32 0) :| [])
                                        )
                                      ]
                                  )
                                  Nothing
                                :| []
                            )
                        )
                    )
                )
            )
        )
    )
  , ( DFunction
        "flatten"
        ( Function
            ()
            (With [] (TIntrinsic (IList (TVariable (TypeIndex KType 4)))))
            ( PAnnotation
                ()
                (TApplication () (TConstructor () "Tree") (TVariable (Parameter () "a") :| []))
                ( PVariable
                    ()
                    ( Label
                        ( TApplication
                            KType
                            (TConstructor (KArrow KType KType) "Tree")
                            (TVariable (TypeIndex KType 4) :| [])
                        )
                        "tree"
                    )
                )
                :| []
            )
            ( EFold
                ()
                (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
                ( EVariable
                    ()
                    ( Label
                        ( TApplication
                            KType
                            (TConstructor (KArrow KType KType) "Tree")
                            (TVariable (TypeIndex KType 4) :| [])
                        )
                        "tree"
                    )
                    :| []
                )
                ( EClause
                    ()
                    ( PConstructor
                        ()
                        ( Label
                            ( TApplication
                                KType
                                (TConstructor (KArrow KType KType) "Tree")
                                (TVariable (TypeIndex KType 4) :| [])
                            )
                            "Node"
                        )
                        [ PVariable () (Label (TVariable (TypeIndex KType 4)) "y")
                        , PAtVariable
                            ()
                            ( Label
                                ( TApplication
                                    KType
                                    (TConstructor (KArrow KType KType) "Tree")
                                    (TVariable (TypeIndex KType 4) :| [])
                                )
                                "lhs"
                            )
                        , PAtVariable
                            ()
                            ( Label
                                ( TApplication
                                    KType
                                    (TConstructor (KArrow KType KType) "Tree")
                                    (TVariable (TypeIndex KType 4) :| [])
                                )
                                "rhs"
                            )
                        ]
                    )
                    ( CPlain
                        ()
                        []
                        ( EApplication
                            ()
                            (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
                            ( EBinaryOperator
                                ()
                                ( (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
                                    `TArrow` (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
                                    `TArrow` (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
                                )
                                OListConcatenation
                            )
                            ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 4)))) "lhs")
                                <| EListCons
                                  ()
                                  (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
                                  (EVariable () (Label (TVariable (TypeIndex KType 4)) "y"))
                                  (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 4)))) "rhs"))
                                :| []
                            )
                        )
                        :| []
                    )
                    <| EClause
                      ()
                      ( PConstructor
                          ()
                          ( Label
                              ( TApplication
                                  KType
                                  (TConstructor (KArrow KType KType) "Tree")
                                  (TVariable (TypeIndex KType 4) :| [])
                              )
                              "Leaf"
                          )
                          []
                      )
                      ( CPlain
                          ()
                          []
                          (EListLiteral () (TIntrinsic (IList (TVariable (TypeIndex KType 4)))) [])
                          :| []
                      )
                    :| []
                )
                ( Just
                    ( ERecursiveLet
                        ()
                        ( PVariable
                            ()
                            ( Label
                                ( ( TApplication
                                      KType
                                      (TConstructor (KArrow KType KType) "Tree")
                                      (TVariable (TypeIndex KType 3) :| [])
                                  )
                                    `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                                )
                                "$fold.2"
                            )
                        )
                        ( ELambda
                            ()
                            ( PVariable
                                ()
                                ( Label
                                    ( TApplication
                                        KType
                                        (TConstructor (KArrow KType KType) "Tree")
                                        (TVariable (TypeIndex KType 3) :| [])
                                    )
                                    "$fold.2.expr"
                                )
                                :| []
                            )
                            ( EMatch
                                ()
                                (TIntrinsic (IList (TVariable (TypeIndex KType 3))))
                                ( EVariable
                                    ()
                                    ( Label
                                        ( TApplication
                                            KType
                                            (TConstructor (KArrow KType KType) "Tree")
                                            (TVariable (TypeIndex KType 3) :| [])
                                        )
                                        "$fold.2.expr"
                                    )
                                )
                                ( EClause
                                    ()
                                    ( PConstructor
                                        ()
                                        ( Label
                                            ( TApplication
                                                KType
                                                (TConstructor (KArrow KType KType) "Tree")
                                                (TVariable (TypeIndex KType 3) :| [])
                                            )
                                            "Node"
                                        )
                                        [ PVariable () (Label (TVariable (TypeIndex KType 3)) "y")
                                        , PVariable
                                            ()
                                            ( Label
                                                ( TApplication
                                                    KType
                                                    (TConstructor (KArrow KType KType) "Tree")
                                                    (TVariable (TypeIndex KType 3) :| [])
                                                )
                                                "lhs"
                                            )
                                        , PVariable
                                            ()
                                            ( Label
                                                ( TApplication
                                                    KType
                                                    (TConstructor (KArrow KType KType) "Tree")
                                                    (TVariable (TypeIndex KType 3) :| [])
                                                )
                                                "rhs"
                                            )
                                        ]
                                    )
                                    ( CPlain
                                        ()
                                        []
                                        ( EApplication
                                            ()
                                            (TIntrinsic (IList (TVariable (TypeIndex KType 3))))
                                            ( EBinaryOperator
                                                ()
                                                ( TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                                                    `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                                                    `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                                                )
                                                OListConcatenation
                                            )
                                            ( EApplication
                                                ()
                                                (TIntrinsic (IList (TVariable (TypeIndex KType 3))))
                                                ( EVariable
                                                    ()
                                                    ( Label
                                                        ( ( TApplication
                                                              KType
                                                              (TConstructor (KArrow KType KType) "Tree")
                                                              (TVariable (TypeIndex KType 3) :| [])
                                                          )
                                                            `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                                                        )
                                                        "$fold.2"
                                                    )
                                                )
                                                ( EVariable
                                                    ()
                                                    ( Label
                                                        ( TApplication
                                                            KType
                                                            (TConstructor (KArrow KType KType) "Tree")
                                                            (TVariable (TypeIndex KType 3) :| [])
                                                        )
                                                        "lhs"
                                                    )
                                                    :| []
                                                )
                                                <| EListCons
                                                  ()
                                                  (TIntrinsic (IList (TVariable (TypeIndex KType 3))))
                                                  (EVariable () (Label (TVariable (TypeIndex KType 3)) "y"))
                                                  ( EApplication
                                                      ()
                                                      (TIntrinsic (IList (TVariable (TypeIndex KType 3))))
                                                      ( EVariable
                                                          ()
                                                          ( Label
                                                              ( ( TApplication
                                                                    KType
                                                                    (TConstructor (KArrow KType KType) "Tree")
                                                                    (TVariable (TypeIndex KType 3) :| [])
                                                                )
                                                                  `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                                                              )
                                                              "$fold.2"
                                                          )
                                                      )
                                                      ( EVariable
                                                          ()
                                                          ( Label
                                                              ( TApplication
                                                                  KType
                                                                  (TConstructor (KArrow KType KType) "Tree")
                                                                  (TVariable (TypeIndex KType 3) :| [])
                                                              )
                                                              "rhs"
                                                          )
                                                          :| []
                                                      )
                                                  )
                                                :| []
                                            )
                                        )
                                        :| []
                                    )
                                    <| EClause
                                      ()
                                      ( PConstructor
                                          ()
                                          ( Label
                                              ( TApplication
                                                  KType
                                                  (TConstructor (KArrow KType KType) "Tree")
                                                  (TVariable (TypeIndex KType 3) :| [])
                                              )
                                              "Leaf"
                                          )
                                          []
                                      )
                                      (CPlain () [] (EListLiteral () (TIntrinsic (IList (TVariable (TypeIndex KType 3)))) []) :| [])
                                    :| []
                                )
                            )
                        )
                        ( EApplication
                            ()
                            (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
                            ( EVariable
                                ()
                                ( Label
                                    ( ( TApplication
                                          KType
                                          (TConstructor (KArrow KType KType) "Tree")
                                          (TVariable (TypeIndex KType 4) :| [])
                                      )
                                        `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 4)))
                                    )
                                    "$fold.2"
                                )
                            )
                            ( EVariable
                                ()
                                ( Label
                                    ( TApplication
                                        KType
                                        (TConstructor (KArrow KType KType) "Tree")
                                        (TVariable (TypeIndex KType 4) :| [])
                                    )
                                    "tree"
                                )
                                :| []
                            )
                        )
                    )
                )
            )
        )
    )
  , ( DConstant
        "sort"
        ( Constant
            ()
            ( With
                []
                ( TIntrinsic (IList (TVariable (TypeIndex KType 5)))
                    `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 5)))
                )
            )
            ( EApplication
                ()
                ( TIntrinsic (IList (TVariable (TypeIndex KType 5)))
                    `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 5)))
                )
                ( EBinaryOperator
                    ()
                    ( ( ( TApplication
                            KType
                            (TConstructor (KArrow KType KType) "Tree")
                            (TVariable (TypeIndex KType 5) :| [])
                        )
                          `TArrow` (TIntrinsic (IList (TVariable (TypeIndex KType 5))))
                      )
                        `TArrow` ( (TIntrinsic (IList (TVariable (TypeIndex KType 5))))
                                    `TArrow` ( TApplication
                                                KType
                                                (TConstructor (KArrow KType KType) "Tree")
                                                (TVariable (TypeIndex KType 5) :| [])
                                             )
                                 )
                        `TArrow` ( (TIntrinsic (IList (TVariable (TypeIndex KType 5))))
                                    `TArrow` (TIntrinsic (IList (TVariable (TypeIndex KType 5))))
                                 )
                    )
                    OReverseComposition
                )
                ( EVariable
                    ()
                    ( Label
                        ( ( TApplication
                              KType
                              (TConstructor (KArrow KType KType) "Tree")
                              (TVariable (TypeIndex KType 5) :| [])
                          )
                            `TArrow` (TIntrinsic (IList (TVariable (TypeIndex KType 5))))
                        )
                        "flatten"
                    )
                    <| EVariable
                      ()
                      ( Label
                          ( (TIntrinsic (IList (TVariable (TypeIndex KType 5))))
                              `TArrow` ( TApplication
                                          KType
                                          (TConstructor (KArrow KType KType) "Tree")
                                          (TVariable (TypeIndex KType 5) :| [])
                                       )
                          )
                          "from_list"
                      )
                    :| []
                )
            )
        )
    )
  ]

fixture6 =
  [ DInstance
      "Ordered"
      (TIntrinsic IInt32)
      [ DFunction
          "compare"
          ( Function
              ()
              (With [] ())
              ( PVariable () (Label () "x")
                  <| PVariable () (Label () "y")
                  :| []
              )
              ( EIf
                  ()
                  ()
                  ( EApplication
                      ()
                      ()
                      (EBinaryOperator () () OLessThan)
                      ( EVariable () (Label () "x")
                          <| EVariable () (Label () "y")
                          :| []
                      )
                  )
                  (EConstructor () (Label () "LessThan"))
                  ( EIf
                      ()
                      ()
                      ( EApplication
                          ()
                          ()
                          (EBinaryOperator () () OGreaterThan)
                          ( EVariable () (Label () "x")
                              <| EVariable () (Label () "y")
                              :| []
                          )
                      )
                      (EConstructor () (Label () "GreaterThan"))
                      (EConstructor () (Label () "EqualTo"))
                  )
              )
          )
      ]
  ]

-- fixture6 =
--  [ ( DFunction
--        "from_list"
--        ( Function
--            ()
--            (With [] ())
--            (PVariable () (Label () "list") :| [])
--            ( ERecursiveLet
--                ()
--                (PVariable () (Label () "$fold.1"))
--                ( ELambda
--                    ()
--                    (PVariable () (Label () "$fold.1.expr") :| [])
--                    ( EMatch
--                        ()
--                        ()
--                        (EVariable () (Label () "$fold.1.expr"))
--                        ( --                          EClause
--                          --                            ()
--                          --                            (PListCons () () (PVariable () (Label () "p")) (PVariable () (Label () "g")))
--                          --                            ( CPlain
--                          --                                ()
--                          --                                []
--                          --                                (EVariable () (Label () "ttt"))
--                          --                                :| []
--                          --                            )
--                          --                            <|
--                          EClause
--                            ()
--                            (PListLiteral () () [])
--                            ( CPlain
--                                ()
--                                []
--                                ( EApplication
--                                    ()
--                                    ()
--                                    (EVariable () (Label () "always"))
--                                    (EConstructor () (Label () "Leaf") :| [])
--                                )
--                                :| []
--                            )
--                            :| []
--                        )
--                    )
--                )
--                ( EApplication
--                    ()
--                    ()
--                    (EVariable () (Label () "$fold.1"))
--                    ( EVariable () (Label () "list")
--                        <| ERecord
--                          ()
--                          ()
--                          ( Map.fromList
--                              [
--                                ( "max"
--                                , EApplication
--                                    ()
--                                    ()
--                                    (EVariable () (Label () "from_int32"))
--                                    (ELiteral () (LInt32 (-1)) :| [])
--                                )
--                              ,
--                                ( "min"
--                                , EApplication
--                                    ()
--                                    ()
--                                    (EVariable () (Label () "from_int32"))
--                                    (ELiteral () (LInt32 0) :| [])
--                                )
--                              ]
--                          )
--                          Nothing
--                        :| []
--                    )
--                )
--            )
--            --                     ( ERecursiveLet
--            --                         ()
--            --                         (PVariable () (Label () "$fold.1"))
--            --                         ( ELambda
--            --                             ()
--            --                             (PVariable () (Label () "$fold.1.expr") :| [])
--            --                             ( EMatch
--            --                                 ()
--            --                                 ()
--            --                                 (EVariable () (Label () "$fold.1.expr"))
--            --                                 ( EClause
--            --                                     ()
--            --                                     (PListCons () () (PVariable () (Label () "p")) (PVariable () (Label () "g")))
--            --                                     ( CPlain
--            --                                         ()
--            --                                         []
--            --                                         ( ELambda
--            --                                             ()
--            --                                             (PVariable () (Label () "range") :| [])
--            --                                             ( EIf
--            --                                                 ()
--            --                                                 ()
--            --                                                 ( EApplication
--            --                                                     ()
--            --                                                     ()
--            --                                                     (EBinaryOperator () () OReverseApplication)
--            --                                                     ( EVariable () (Label () "p")
--            --                                                         <| EApplication
--            --                                                           ()
--            --                                                           ()
--            --                                                           (EVariable () (Label () "in_range"))
--            --                                                           (EVariable () (Label () "range") :| [])
--            --                                                         :| []
--            --                                                     )
--            --                                                 )
--            --                                                 ( EApplication
--            --                                                     ()
--            --                                                     ()
--            --                                                     (EConstructor () (Label () "Node"))
--            --                                                     ( EVariable () (Label () "p")
--            --                                                         <| EApplication
--            --                                                           ()
--            --                                                           ()
--            --                                                           (EVariable () (Label () "$fold.1"))
--            --                                                           ( EVariable () (Label () "g")
--            --                                                               :| [ ERecord
--            --                                                                     ()
--            --                                                                     ()
--            --                                                                     ( Map.fromList
--            --                                                                         [
--            --                                                                           ( "max"
--            --                                                                           , EVariable () (Label () "p")
--            --                                                                           )
--            --                                                                         ,
--            --                                                                           ( "min"
--            --                                                                           , ESelect
--            --                                                                               ()
--            --                                                                               (Label () "min")
--            --                                                                               (EVariable () (Label () "range"))
--            --                                                                           )
--            --                                                                         ]
--            --                                                                     )
--            --                                                                     Nothing
--            --                                                                  ]
--            --                                                           )
--            --                                                         <| EApplication
--            --                                                           ()
--            --                                                           ()
--            --                                                           (EVariable () (Label () "$fold.1"))
--            --                                                           ( EVariable () (Label () "g")
--            --                                                               <| ERecord
--            --                                                                 ()
--            --                                                                 ()
--            --                                                                 ( Map.fromList
--            --                                                                     [
--            --                                                                       ( "max"
--            --                                                                       , ESelect
--            --                                                                           ()
--            --                                                                           (Label () "max")
--            --                                                                           (EVariable () (Label () "range"))
--            --                                                                       )
--            --                                                                     ,
--            --                                                                       ( "min"
--            --                                                                       , EVariable () (Label () "p")
--            --                                                                       )
--            --                                                                     ]
--            --                                                                 )
--            --                                                                 Nothing
--            --                                                               :| []
--            --                                                           )
--            --                                                         :| []
--            --                                                     )
--            --                                                 )
--            --                                                 ( EApplication
--            --                                                     ()
--            --                                                     ()
--            --                                                     (EVariable () (Label () "$fold.1"))
--            --                                                     ( EVariable () (Label () "g")
--            --                                                         <| EVariable () (Label () "range")
--            --                                                         :| []
--            --                                                     )
--            --                                                 )
--            --                                             )
--            --                                         )
--            --                                         :| []
--            --                                     )
--            --                                     <| EClause
--            --                                       ()
--            --                                       (PListLiteral () () [])
--            --                                       ( CPlain
--            --                                           ()
--            --                                           []
--            --                                           ( EApplication
--            --                                               ()
--            --                                               ()
--            --                                               (EVariable () (Label () "always"))
--            --                                               ( EConstructor () (Label () "Leaf")
--            --                                                   :| []
--            --                                               )
--            --                                           )
--            --                                           :| []
--            --                                       )
--            --                                     :| []
--            --                                 )
--            --                             )
--            --                         )
--            --                         ( EApplication
--            --                             ()
--            --                             ()
--            --                             (EVariable () (Label () "$fold.1"))
--            --                             ( EVariable () (Label () "list")
--            --                                 <| ERecord
--            --                                   ()
--            --                                   ()
--            --                                   ( Map.fromList
--            --                                       [
--            --                                         ( "max"
--            --                                         , EApplication
--            --                                             ()
--            --                                             ()
--            --                                             (EVariable () (Label () "from_int32"))
--            --                                             (ELiteral () (LInt32 (-1)) :| [])
--            --                                         )
--            --                                       ,
--            --                                         ( "min"
--            --                                         , EApplication
--            --                                             ()
--            --                                             ()
--            --                                             (EVariable () (Label () "from_int32"))
--            --                                             (ELiteral () (LInt32 0) :| [])
--            --                                         )
--            --                                       ]
--            --                                   )
--            --                                   Nothing
--            --                                 :| []
--            --                             )
--            --                         )
--            --                     )
--            --                 )
--        )
--    )
--  ]
--
-- fixture7 =
--  ERecursiveLet
--    ()
--    (PVariable () (Label () "$fold.1"))
--    (EVariable () (Label () "x"))
--    --    ( ELambda
--    --        ()
--    --        (PVariable () (Label () "$fold.1.expr") :| [])
--    --        ( EMatch
--    --            ()
--    --            ()
--    --            (EVariable () (Label () "$fold.1.expr"))
--    --            ( --                          EClause
--    --              --                            ()
--    --              --                            (PListCons () () (PVariable () (Label () "p")) (PVariable () (Label () "g")))
--    --              --                            ( CPlain
--    --              --                                ()
--    --              --                                []
--    --              --                                (EVariable () (Label () "ttt"))
--    --              --                                :| []
--    --              --                            )
--    --              --                            <|
--    --              EClause
--    --                ()
--    --                (PListLiteral () () [])
--    --                ( CPlain
--    --                    ()
--    --                    []
--    --                    ( EApplication
--    --                        ()
--    --                        ()
--    --                        (EVariable () (Label () "always"))
--    --                        (EConstructor () (Label () "Leaf") :| [])
--    --                    )
--    --                    :| []
--    --                )
--    --                :| []
--    --            )
--    --        )
--    --    )
--    ( EApplication
--        ()
--        ()
--        (EVariable () (Label () "$fold.1"))
--        ( EVariable () (Label () "list")
--            <| ERecord
--              ()
--              ()
--              ( Map.fromList
--                  [
--                    ( "max"
--                    , EApplication
--                        ()
--                        ()
--                        (EVariable () (Label () "from_int32"))
--                        (ELiteral () (LInt32 (-1)) :| [])
--                    )
--                  ,
--                    ( "min"
--                    , EApplication
--                        ()
--                        ()
--                        (EVariable () (Label () "from_int32"))
--                        (ELiteral () (LInt32 0) :| [])
--                    )
--                  ]
--              )
--              Nothing
--            :| []
--        )
--    )
--
-- fixture8 =
--  ELambda
--    ()
--    (PVariable () (Label () "x") :| [])
--    ( ERecursiveLet
--        ()
--        (PVariable () (Label () "$fold.1"))
--        ( ELambda
--            ()
--            (PVariable () (Label () "$fold.1.expr") :| [])
--            ( EMatch
--                ()
--                ()
--                (EVariable () (Label () "$fold.1.expr"))
--                ( EClause
--                    ()
--                    (PListCons () () (PVariable () (Label () "p")) (PVariable () (Label () "g")))
--                    ( CPlain
--                        ()
--                        []
--                        ( ELambda
--                            ()
--                            (PVariable () (Label () "range") :| [])
--                            ( EIf
--                                ()
--                                ()
--                                ( EApplication
--                                    ()
--                                    ()
--                                    (EBinaryOperator () () OReverseApplication)
--                                    ( EVariable () (Label () "p")
--                                        <| EApplication
--                                          ()
--                                          ()
--                                          (EVariable () (Label () "in_range"))
--                                          (EVariable () (Label () "range") :| [])
--                                        :| []
--                                    )
--                                )
--                                ( EApplication
--                                    ()
--                                    ()
--                                    (EConstructor () (Label () "Node"))
--                                    ( EVariable () (Label () "p")
--                                        <| EApplication
--                                          ()
--                                          ()
--                                          (EVariable () (Label () "$fold.1"))
--                                          ( EVariable () (Label () "g")
--                                              :| [ ERecord
--                                                    ()
--                                                    ()
--                                                    ( Map.fromList
--                                                        [
--                                                          ( "max"
--                                                          , EVariable () (Label () "p")
--                                                          )
--                                                        ,
--                                                          ( "min"
--                                                          , ESelect
--                                                              ()
--                                                              (Label () "min")
--                                                              (EVariable () (Label () "range"))
--                                                          )
--                                                        ]
--                                                    )
--                                                    Nothing
--                                                 ]
--                                          )
--                                        <| EApplication
--                                          ()
--                                          ()
--                                          (EVariable () (Label () "$fold.1"))
--                                          ( EVariable () (Label () "g")
--                                              <| ERecord
--                                                ()
--                                                ()
--                                                ( Map.fromList
--                                                    [
--                                                      ( "max"
--                                                      , ESelect
--                                                          ()
--                                                          (Label () "max")
--                                                          (EVariable () (Label () "range"))
--                                                      )
--                                                    ,
--                                                      ( "min"
--                                                      , EVariable () (Label () "p")
--                                                      )
--                                                    ]
--                                                )
--                                                Nothing
--                                              :| []
--                                          )
--                                        :| []
--                                    )
--                                )
--                                ( EApplication
--                                    ()
--                                    ()
--                                    (EVariable () (Label () "$fold.1"))
--                                    ( EVariable () (Label () "g")
--                                        <| EVariable () (Label () "range")
--                                        :| []
--                                    )
--                                )
--                            )
--                        )
--                        :| []
--                    )
--                    <| EClause
--                      ()
--                      (PListLiteral () () [])
--                      ( CPlain
--                          ()
--                          []
--                          ( EApplication
--                              ()
--                              ()
--                              (EVariable () (Label () "always"))
--                              (EConstructor () (Label () "Leaf") :| [])
--                          )
--                          :| []
--                      )
--                    :| []
--                )
--            )
--        )
--        ( EApplication
--            ()
--            ()
--            (EVariable () (Label () "$fold.1"))
--            ( EVariable () (Label () "list")
--                <| ERecord
--                  ()
--                  ()
--                  ( Map.fromList
--                      [
--                        ( "max"
--                        , EApplication
--                            ()
--                            ()
--                            (EVariable () (Label () "from_int32"))
--                            (ELiteral () (LInt32 (-1)) :| [])
--                        )
--                      ,
--                        ( "min"
--                        , EApplication
--                            ()
--                            ()
--                            (EVariable () (Label () "from_int32"))
--                            (ELiteral () (LInt32 0) :| [])
--                        )
--                      ]
--                  )
--                  Nothing
--                :| []
--            )
--        )
--    )
