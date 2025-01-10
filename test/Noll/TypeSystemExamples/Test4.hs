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
  Intrinsic (..),
  Kind (..),
  Pattern (..),
  Primitive (..),
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
-- let
--   from_list =
--     fn(list : List(a)) =>
--       fold(list, { max = from_int32(-1), min = from_int32(0) }) {
--         | p :: @g =>
--             fn(range) =>
--               if p |.in_range(range)
--                 then
--                   Node(p
--                   , g({ min = range.min, max = p })
--                   , g({ min = p, max = range.max })
--                   )
--                 else
--                   g(range)
--         | [] =>
--             fn(_) =>
--               Leaf
--       }
--   in
--     from_list
--
fixture :: Expression () ()
fixture =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label () "from_list"))
        ( ELambda
            ()
            (PAnnotation () (TIntrinsic (IList (TVariable (TypeParam () "a")))) (PVariable () (Label () "list")) :| [])
            ( EFold
                ()
                ()
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
                                    (EBinaryOperator () ((), OForwardApplication))
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
                                                    ( "max"
                                                    , EVariable () (Label () "p")
                                                    )
                                                  ,
                                                    ( "min"
                                                    , ESelect () () (Label () "min") (EVariable () (Label () "range"))
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
                                                    ( "max"
                                                    , ESelect () () (Label () "max") (EVariable () (Label () "range"))
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
                                    (EVariable () (Label () "g"))
                                    (EVariable () (Label () "range") :| [])
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
                          ( ELambda
                              ()
                              (PAny () () :| [])
                              (EConstructor () (Label () "Leaf"))
                          )
                          :| []
                      )
                      :| []
                )
                ( Just
                    ( ELet
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
                                                                        , ("min", ESelect () () (Label () "min") (EVariable () (Label () "range")))
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
                                                                        [ ("max", ESelect () () (Label () "max") (EVariable () (Label () "range")))
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
                    )
                )
            )
        )
        :| []
    )
    (EVariable () (Label () "from_list"))
