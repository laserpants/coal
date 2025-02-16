{-# LANGUAGE OverloadedStrings #-}

module Noll.SystemFExamples.Test05 where

import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Noll.Compiler
import Noll.Label (Label (..))
import Noll.Language (
  BinaryOperator (..),
  Binding (..),
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
  Type (..),
  TypeIndex (..),
 )
import Noll.SystemFSpec.TestRunner
import Test.Hspec (Spec, describe, hspec, it)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Noll.Common.Environment as Environment

tvariable :: Int -> IndexedType
tvariable n = TVariable (TypeIndex KType n)

listType :: Int -> IndexedType
listType n = TIntrinsic (IList (tvariable n))

treeType :: Int -> IndexedType
treeType n = TApplication KType (TConstructor (KArrow KType KType) "Tree") (tvariable n :| [])

maxMinType :: Int -> IndexedType
maxMinType n = TIntrinsic (IRecord (TRow (RExtend "max" (tvariable n) (RExtend "min" (tvariable n) RNil))))

bool :: IndexedType
bool = TIntrinsic IBool

int32 :: IndexedType
int32 = TIntrinsic IInt32

tvariable0 :: IndexedType
tvariable0 = tvariable 0

list0Type :: IndexedType
list0Type = listType 0

tree0Type :: IndexedType
tree0Type = treeType 0

maxMin0Type :: IndexedType
maxMin0Type = maxMinType 0

spec :: Spec
spec =
  describe "" $
    it "" $ do
      testResultExpression (runTest fixture) == fixture1

runTest :: (Show a, Eq a, Data a) => Expression a () -> TestResult (Expression a (Type TypeIndex Kind)) a
runTest =
  runTypedExpressionTest
    (CompilerEnvironment env1 env2)
    [
      ( "list"
      , Forall
          (Set.fromList [TypeIndex KType 0])
          []
          ( TIntrinsic (IList tvariable0)
          )
      )
    ,
      ( "from_int32"
      , Forall
          (Set.fromList [TypeIndex KType 0])
          []
          ( TIntrinsic IInt32 `TArrow` tvariable0
          )
      )
    ,
      ( "in_range"
      , ( Forall
            (Set.fromList [TypeIndex KType 0])
            []
            ( TIntrinsic
                ( IRecord
                    ( TRow
                        ( RExtend
                            "max"
                            (TVariable (TypeIndex KType 0))
                            ( RExtend "min" (TVariable (TypeIndex KType 0)) RNil
                            )
                        )
                    )
                )
                `TArrow` TVariable (TypeIndex KType 0)
                `TArrow` TIntrinsic IBool
            )
        )
      )
    ]
 where
  env1 =
    Environment.fromList
      [
        ( "Leaf"
        , Constructor
            "Leaf"
            0
            ( Forall
                (Set.fromList [TypeIndex KType 0])
                []
                ( TApplication
                    KType
                    (TConstructor (KArrow KType KType) "Tree")
                    (TVariable (TypeIndex KType 0) :| [])
                )
            )
        )
      ,
        ( "Node"
        , Constructor
            "Node"
            3
            ( Forall
                (Set.fromList [TypeIndex KType 0])
                []
                ( TVariable (TypeIndex KType 0)
                    `TArrow` ( TApplication
                                KType
                                (TConstructor (KArrow KType KType) "Tree")
                                (TVariable (TypeIndex KType 0) :| [])
                             )
                    `TArrow` ( TApplication
                                KType
                                (TConstructor (KArrow KType KType) "Tree")
                                (TVariable (TypeIndex KType 0) :| [])
                             )
                    `TArrow` ( TApplication
                                KType
                                (TConstructor (KArrow KType KType) "Tree")
                                (TVariable (TypeIndex KType 0) :| [])
                             )
                )
            )
        )
      ]
  env2 =
    Environment.fromList
      [ ("Ordering", KType)
      ,
        ( "Tree"
        , KArrow KType KType
        )
      ]

fixture :: Expression () ()
fixture =
  ERecursiveLet
    ()
    (PVariable () (Label () "$fold:1"))
    ( ELambda
        ()
        (PVariable () (Label () "$fold:1:expr") :| [])
        ( EMatch
            ()
            ()
            (EVariable () (Label () "$fold:1:expr"))
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
                                    :| [ EApplication
                                          ()
                                          ()
                                          (EVariable () (Label () "in_range"))
                                          (EVariable () (Label () "range") :| [])
                                       ]
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
                                      (EVariable () (Label () "$fold:1"))
                                      ( EVariable () (Label () "g")
                                          <| ERecord
                                            ()
                                            ()
                                            ( Map.fromList
                                                [ ("max", EVariable () (Label () "p"))
                                                , ("min", ESelect () (Label () "min") (EVariable () (Label () "range")))
                                                ]
                                            )
                                            Nothing
                                            :| []
                                      )
                                    <| EApplication
                                      ()
                                      ()
                                      (EVariable () (Label () "$fold:1"))
                                      ( EVariable () (Label () "g")
                                          <| ERecord
                                            ()
                                            ()
                                            ( Map.fromList
                                                [ ("max", ESelect () (Label () "max") (EVariable () (Label () "range")))
                                                , ("min", EVariable () (Label () "p"))
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
                                (EVariable () (Label () "$fold:1"))
                                (EVariable () (Label () "g") <| EVariable () (Label () "range") :| [])
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
                      (ELambda () (PAny () () :| []) (EConstructor () (Label () "Leaf")))
                      :| []
                  )
                  :| []
            )
        )
    )
    ( EApplication
        ()
        ()
        (EVariable () (Label () "$fold:1"))
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

fixture1 :: Expression () (Type TypeIndex Kind)
fixture1 =
  ERecursiveLet
    ()
    (PVariable () (Label (list0Type `TArrow` maxMin0Type `TArrow` tree0Type) "$fold:1"))
    ( ELambda
        ()
        (PVariable () (Label list0Type "$fold:1:expr") :| [])
        ( EMatch
            ()
            (maxMin0Type `TArrow` tree0Type)
            (EVariable () (Label list0Type "$fold:1:expr"))
            ( EClause
                ()
                ( PListCons
                    ()
                    list0Type
                    (PVariable () (Label tvariable0 "p"))
                    (PVariable () (Label list0Type "g"))
                )
                ( CPlain
                    ()
                    []
                    ( ELambda
                        ()
                        ( PVariable
                            ()
                            (Label maxMin0Type "range")
                            :| []
                        )
                        ( EIf
                            ()
                            tree0Type
                            ( EApplication
                                ()
                                bool
                                ( EBinaryOperator
                                    ()
                                    ( tvariable0 `TArrow` (tvariable0 `TArrow` bool) `TArrow` bool
                                    )
                                    OReverseApplication
                                )
                                ( EVariable () (Label tvariable0 "p")
                                    :| [ EApplication
                                          ()
                                          (tvariable0 `TArrow` bool)
                                          ( EVariable
                                              ()
                                              ( Label
                                                  (maxMin0Type `TArrow` tvariable0 `TArrow` bool)
                                                  "in_range"
                                              )
                                          )
                                          (EVariable () (Label maxMin0Type "range") :| [])
                                       ]
                                )
                            )
                            ( EApplication
                                ()
                                tree0Type
                                (EConstructor () (Label (tvariable0 `TArrow` tree0Type `TArrow` tree0Type `TArrow` tree0Type) "Node"))
                                ( EVariable () (Label tvariable0 "p")
                                    <| EApplication
                                      ()
                                      tree0Type
                                      ( EVariable
                                          ()
                                          ( Label
                                              (list0Type `TArrow` maxMin0Type `TArrow` tree0Type)
                                              "$fold:1"
                                          )
                                      )
                                      ( EVariable () (Label list0Type "g")
                                          <| ERecord
                                            ()
                                            maxMin0Type
                                            ( Map.fromList
                                                [ ("max", EVariable () (Label tvariable0 "p"))
                                                ,
                                                  ( "min"
                                                  , ESelect
                                                      ()
                                                      (Label tvariable0 "min")
                                                      (EVariable () (Label maxMin0Type "range"))
                                                  )
                                                ]
                                            )
                                            Nothing
                                            :| []
                                      )
                                    <| EApplication
                                      ()
                                      tree0Type
                                      (EVariable () (Label (list0Type `TArrow` maxMin0Type `TArrow` tree0Type) "$fold:1"))
                                      ( EVariable () (Label list0Type "g")
                                          <| ERecord
                                            ()
                                            maxMin0Type
                                            ( Map.fromList
                                                [ ("max", ESelect () (Label tvariable0 "max") (EVariable () (Label maxMin0Type "range")))
                                                , ("min", EVariable () (Label tvariable0 "p"))
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
                                tree0Type
                                (EVariable () (Label (list0Type `TArrow` maxMin0Type `TArrow` tree0Type) "$fold:1"))
                                (EVariable () (Label list0Type "g") <| EVariable () (Label maxMin0Type "range") :| [])
                            )
                        )
                    )
                    :| []
                )
                <| EClause
                  ()
                  (PListLiteral () list0Type [])
                  ( CPlain
                      ()
                      []
                      (ELambda () (PAny () maxMin0Type :| []) (EConstructor () (Label tree0Type "Leaf")))
                      :| []
                  )
                  :| []
            )
        )
    )
    ( EApplication
        ()
        (treeType 1)
        (EVariable () (Label (listType 1 `TArrow` maxMinType 1 `TArrow` treeType 1) "$fold:1"))
        ( EVariable () (Label (listType 1) "list")
            <| ERecord
              ()
              (maxMinType 1)
              ( Map.fromList
                  [
                    ( "max"
                    , EApplication
                        ()
                        (tvariable 1)
                        (EVariable () (Label (int32 `TArrow` tvariable 1) "from_int32"))
                        (ELiteral () (LInt32 (-1)) :| [])
                    )
                  ,
                    ( "min"
                    , EApplication
                        ()
                        (tvariable 1)
                        (EVariable () (Label (int32 `TArrow` tvariable 1) "from_int32"))
                        (ELiteral () (LInt32 0) :| [])
                    )
                  ]
              )
              Nothing
              :| []
        )
    )
