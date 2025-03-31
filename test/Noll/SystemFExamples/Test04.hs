{-# LANGUAGE OverloadedStrings #-}

module Noll.SystemFExamples.Test04 where

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
import qualified Lang.Common.Environment as Environment

tvariable0 :: IndexedType
tvariable0 = TVariable (TypeIndex KType 0)

bool :: IndexedType
bool = TIntrinsic IBool

int32 :: IndexedType
int32 = TIntrinsic IInt32

list0Type :: IndexedType
list0Type = TIntrinsic (IList tvariable0)

tree0Type :: IndexedType
tree0Type = TApplication KType (TConstructor (KArrow KType KType) "Tree") (tvariable0 :| [])

maxMin0Type :: IndexedType
maxMin0Type = TIntrinsic (IRecord (TRow (RExtend "max" tvariable0 (RExtend "min" tvariable0 RNil))))

spec :: Spec
spec =
  describe "fn(range) => if p |.in_range(range) then Node(p , g({ min = range.min, max = p }) , g({ min = p, max = range.max })) else g(range)" $
    it "" $ do
      testResultExpression (runTest fixture)
        == ELambda
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

runTest :: (Show a, Eq a, Data a) => Expression a () -> TestResult (Expression a (Type TypeIndex Kind)) a
runTest =
  runTypedExpressionTest
    (CompilerEnvironment env1 env2)
    [
      ( "p"
      , Forall
          (Set.fromList [TypeIndex KType 0])
          []
          ( TVariable (TypeIndex KType 0)
          )
      )
    ,
      ( "g"
      , Forall
          (Set.fromList [TypeIndex KType 0])
          []
          ( TVariable (TypeIndex KType 0)
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
    ,
      ( "$fold:1"
      , Forall
          (Set.fromList [TypeIndex KType 0])
          []
          ( TIntrinsic (IList (TVariable (TypeIndex KType 0)))
              `TArrow` ( TIntrinsic
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
                       )
              `TArrow` ( TApplication
                          KType
                          (TConstructor (KArrow KType KType) "Tree")
                          (TVariable (TypeIndex KType 0) :| [])
                       )
          )
      )
    ]
 where
  env1 =
    Environment.fromList
      [
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
      [
        ( "Tree"
        , KArrow KType KType
        )
      ]

--
-- fn(range) =>
--   if p |.in_range(range)
--     then
--       Node(p
--       , g({ min = range.min, max = p })
--       , g({ min = p, max = range.max })
--       )
--     else
--       g(range)
--
fixture :: Expression () ()
fixture =
  ELambda
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
