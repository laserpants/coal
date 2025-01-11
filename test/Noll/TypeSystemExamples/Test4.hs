{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystemExamples.Test4 where

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
  Pattern (..),
  Primitive (..),
  Row (..),
  Scheme (..),
  Type (..),
  TypeIndex (..),
  TypeParam (..),
 )
import Noll.TypeSystemSpec.TestRunner
import Test.Hspec (Spec, describe, hspec, it)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Noll.Lib.Environment as Environment

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
  describe "" $
    it "" $ do
      testResultExpression (runTest fixture)
        == ELet
          ()
          ( BPattern
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
                                              , OForwardApplication
                                              )
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
              :| []
          )
          ( EApplication
              ()
              tree0Type
              (EVariable () (Label (list0Type `TArrow` maxMin0Type `TArrow` tree0Type) "$fold:1"))
              ( EVariable () (Label list0Type "list")
                  <| ERecord
                    ()
                    maxMin0Type
                    ( Map.fromList
                        [
                          ( "max"
                          , EApplication
                              ()
                              tvariable0
                              (EVariable () (Label (int32 `TArrow` tvariable0) "from_int32"))
                              (ELiteral () (LInt32 (-1)) :| [])
                          )
                        ,
                          ( "min"
                          , EApplication
                              ()
                              tvariable0
                              (EVariable () (Label (int32 `TArrow` tvariable0) "from_int32"))
                              (ELiteral () (LInt32 0) :| [])
                          )
                        ]
                    )
                    Nothing
                    :| []
              )
          )

runTest :: (Eq a) => Expression a () -> TestResult a
runTest =
  runTypedExpressionTest
    (CompilerEnvironment env1 env2)
    []
 where
  env1 =
    Environment.fromList
      []
  env2 =
    Environment.fromList
      [ ("Ordering", KType)
      ]

fixture :: Expression () ()
fixture =
  ELet
    ()
    ( BPattern
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
                                    (EBinaryOperator () ((), OForwardApplication))
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
        :| []
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
