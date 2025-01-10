{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystemExamples.Test4 where

import Data.List.NonEmpty (NonEmpty (..), (<|))
import Noll.Compiler
import Noll.Label (Label (..))
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
import Noll.TypeSystemSpec.TestRunner
import Test.Hspec (Spec, describe, hspec, it)

import qualified Data.Set as Set
import qualified Noll.Lib.Environment as Environment

spec :: Spec
spec =
  describe "" $
    it "" $ do
      testResultExpression (runTest fixture)
        == undefined

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

--
--
fixture :: Expression () ()
fixture =
  undefined

--  ELet
--    ( BPattern
--        (PVariable (Label 191 "from_list"))
--        ( ELambda
--            (PAnnotated (TIntrinsic (IList (TVariable (TypeId () "a")))) (PVariable (Label 59 "list")) :| [])
--            ( EFold
--                60
--                ( EVariable (Label 61 "list")
--                    <| ERecord
--                      62
--                      ( Map.fromList
--                          [
--                            ( "max"
--                            , EApplication
--                                63
--                                (EVariable (Label 64 "from_int32"))
--                                (ELiteral (LInt32 (-1)) :| [])
--                            )
--                          ,
--                            ( "min"
--                            , EApplication
--                                65
--                                (EVariable (Label 66 "from_int32"))
--                                (ELiteral (LInt32 0) :| [])
--                            )
--                          ]
--                      )
--                      Nothing
--                      :| []
--                )
--                ( EClause
--                    ( PListCons
--                        67
--                        (PVariable (Label 68 "p"))
--                        (PAtVariable (Label 69 "g"))
--                        :| []
--                    )
--                    ( CPlain
--                        []
--                        ( ELambda
--                            (PVariable (Label 70 "range") :| [])
--                            ( EIf
--                                ( EApplication
--                                    71
--                                    (EBinaryOperator (72, OForwardApplication))
--                                    ( EVariable (Label 73 "p")
--                                        <| EApplication
--                                          74
--                                          (EVariable (Label 75 "in_range"))
--                                          (EVariable (Label 76 "range") :| [])
--                                          :| []
--                                    )
--                                )
--                                ( EApplication
--                                    77
--                                    (EConstructor (Label 78 "Node"))
--                                    ( EVariable (Label 79 "p")
--                                        <| EApplication
--                                          80
--                                          (EVariable (Label 81 "g"))
--                                          ( ERecord
--                                              82
--                                              ( Map.fromList
--                                                  [
--                                                    ( "max"
--                                                    , EVariable (Label 83 "p")
--                                                    )
--                                                  ,
--                                                    ( "min"
--                                                    , ESelect 84 "min" (EVariable (Label 85 "range"))
--                                                    )
--                                                  ]
--                                              )
--                                              Nothing
--                                              :| []
--                                          )
--                                        <| EApplication
--                                          86
--                                          (EVariable (Label 87 "g"))
--                                          ( ERecord
--                                              88
--                                              ( Map.fromList
--                                                  [
--                                                    ( "max"
--                                                    , ESelect 89 "max" (EVariable (Label 90 "range"))
--                                                    )
--                                                  ,
--                                                    ( "min"
--                                                    , EVariable (Label 91 "p")
--                                                    )
--                                                  ]
--                                              )
--                                              Nothing
--                                              :| []
--                                          )
--                                          :| []
--                                    )
--                                )
--                                (EApplication 92 (EVariable (Label 93 "g")) (EVariable (Label 94 "range") :| []))
--                            )
--                        )
--                        :| []
--                    )
--                    <| EClause
--                      (PListLiteral 95 [] :| [])
--                      ( CPlain
--                          []
--                          (ELambda (PAny 96 :| []) (EConstructor (Label 97 "Leaf")))
--                          :| []
--                      )
--                      :| []
--                )
--                ( Just
--                    ( ELet
--                        ( BPattern
--                            (PVariable (Label 98 "$fold:1"))
--                            ( ELambda
--                                (PVariable (Label 99 "$fold:1:expr") :| [])
--                                ( EMatch
--                                    100
--                                    (EVariable (Label 101 "$fold:1:expr") :| [])
--                                    ( EClause
--                                        (PListCons 102 (PVariable (Label 103 "p")) (PVariable (Label 104 "g")) :| [])
--                                        ( CPlain
--                                            []
--                                            ( ELambda
--                                                (PVariable (Label 105 "range") :| [])
--                                                ( EIf
--                                                    ( EApplication
--                                                        106
--                                                        (EBinaryOperator (107, OForwardApplication))
--                                                        ( EVariable (Label 108 "p")
--                                                            :| [ EApplication
--                                                                  109
--                                                                  (EVariable (Label 110 "in_range"))
--                                                                  (EVariable (Label 111 "range") :| [])
--                                                               ]
--                                                        )
--                                                    )
--                                                    ( EApplication
--                                                        112
--                                                        (EConstructor (Label 113 "Node"))
--                                                        ( EVariable (Label 114 "p")
--                                                            <| EApplication
--                                                              115
--                                                              (EVariable (Label 116 "$fold:1"))
--                                                              ( EVariable (Label 117 "g")
--                                                                  <| ERecord
--                                                                    118
--                                                                    ( Map.fromList
--                                                                        [ ("max", EVariable (Label 119 "p"))
--                                                                        , ("min", ESelect 120 "min" (EVariable (Label 121 "range")))
--                                                                        ]
--                                                                    )
--                                                                    Nothing
--                                                                    :| []
--                                                              )
--                                                            <| EApplication
--                                                              122
--                                                              (EVariable (Label 123 "$fold:1"))
--                                                              ( EVariable (Label 124 "g")
--                                                                  <| ERecord
--                                                                    125
--                                                                    ( Map.fromList
--                                                                        [ ("max", ESelect 126 "max" (EVariable (Label 127 "range")))
--                                                                        , ("min", EVariable (Label 128 "p"))
--                                                                        ]
--                                                                    )
--                                                                    Nothing
--                                                                    :| []
--                                                              )
--                                                              :| []
--                                                        )
--                                                    )
--                                                    ( EApplication
--                                                        129
--                                                        (EVariable (Label 130 "$fold:1"))
--                                                        (EVariable (Label 131 "g") <| EVariable (Label 132 "range") :| [])
--                                                    )
--                                                )
--                                            )
--                                            :| []
--                                        )
--                                        <| EClause
--                                          (PListLiteral 133 [] :| [])
--                                          ( CPlain
--                                              []
--                                              (ELambda (PAny 134 :| []) (EConstructor (Label 135 "Leaf")))
--                                              :| []
--                                          )
--                                          :| []
--                                    )
--                                    Nothing
--                                )
--                            )
--                            :| []
--                        )
--                        ( EApplication
--                            136
--                            (EVariable (Label 137 "$fold:1"))
--                            ( EVariable (Label 138 "list")
--                                <| ERecord
--                                  139
--                                  ( Map.fromList
--                                      [
--                                        ( "max"
--                                        , EApplication
--                                            140
--                                            (EVariable (Label 141 "from_int32"))
--                                            (ELiteral (LInt32 (-1)) :| [])
--                                        )
--                                      ,
--                                        ( "min"
--                                        , EApplication
--                                            142
--                                            (EVariable (Label 143 "from_int32"))
--                                            (ELiteral (LInt32 0) :| [])
--                                        )
--                                      ]
--                                  )
--                                  Nothing
--                                  :| []
--                            )
--                        )
--                    )
--                )
--            )
--        )
--        :| []
--    )
