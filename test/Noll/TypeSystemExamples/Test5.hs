{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystemExamples.Test5 where

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

spec :: Spec
spec =
  describe "" $
    it "" $ do
      testResultExpression (runTest fixture)
        == ( ELet
              ()
              ( BPattern
                  ()
                  (PVariable () (Label undefined "from_list"))
                  ( ELambda
                      ()
                      ( PAnnotation
                          ()
                          (TIntrinsic (IList (TVariable (TypeParam () "a"))))
                          (PVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "list"))
                          :| []
                      )
                      ( EFold
                          ()
                          ( TApplication
                              KType
                              (TConstructor (KArrow KType KType) "Tree")
                              (TVariable (TypeIndex KType 0) :| [])
                          )
                          ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "list")
                              <| ERecord
                                ()
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
                                )
                                ( Map.fromList
                                    [
                                      ( "max"
                                      , EApplication
                                          ()
                                          (TVariable (TypeIndex KType 0))
                                          (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0)) "from_int32"))
                                          (ELiteral () (LInt32 (-1)) :| [])
                                      )
                                    ,
                                      ( "min"
                                      , EApplication
                                          ()
                                          (TVariable (TypeIndex KType 0))
                                          (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0)) "from_int32"))
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
                                  (TIntrinsic (IList (TVariable (TypeIndex KType 0))))
                                  (PVariable () (Label (TVariable (TypeIndex KType 0)) "p"))
                                  (PAtVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "g"))
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
                                                              (TVariable (TypeIndex KType 0))
                                                              ( RExtend "min" (TVariable (TypeIndex KType 0)) RNil
                                                              )
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
                                          undefined
                                          ( EApplication
                                              ()
                                              (TIntrinsic IBool)
                                              ( EBinaryOperator
                                                  ()
                                                  ( TVariable (TypeIndex KType 0)
                                                      `TArrow` (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
                                                      `TArrow` TIntrinsic IBool
                                                  , OForwardApplication
                                                  )
                                              )
                                              ( EVariable () (Label (TVariable (TypeIndex KType 0)) "p")
                                                  <| EApplication
                                                    ()
                                                    (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
                                                    ( EVariable
                                                        ()
                                                        ( Label
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
                                                                            (TVariable (TypeIndex KType 0))
                                                                            ( RExtend "min" (TVariable (TypeIndex KType 0)) RNil
                                                                            )
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
                                                  (TVariable (TypeIndex KType 0) :| [])
                                              )
                                              ( EConstructor
                                                  ()
                                                  ( Label
                                                      ( let tree0 = TApplication KType (TConstructor (KArrow KType KType) "Tree") (TVariable (TypeIndex KType 0) :| [])
                                                         in TVariable (TypeIndex KType 0) `TArrow` tree0 `TArrow` tree0 `TArrow` tree0
                                                      )
                                                      "Node"
                                                  )
                                              )
                                              ( EVariable () (Label (TVariable (TypeIndex KType 0)) "p")
                                                  <| EApplication
                                                    ()
                                                    ( TApplication
                                                        KType
                                                        (TConstructor (KArrow KType KType) "Tree")
                                                        (TVariable (TypeIndex KType 0) :| [])
                                                    )
                                                    (EVariable () (Label undefined "g"))
                                                    ( ERecord
                                                        ()
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
                                                        )
                                                        ( Map.fromList
                                                            [
                                                              ( "max"
                                                              , EVariable () (Label (TVariable (TypeIndex KType 0)) "p")
                                                              )
                                                            ,
                                                              ( "min"
                                                              , ESelect
                                                                  ()
                                                                  (Label (TVariable (TypeIndex KType 0)) "min")
                                                                  (EVariable () (Label undefined "range"))
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
                                                        (TVariable (TypeIndex KType 0) :| [])
                                                    )
                                                    ( EVariable
                                                        ()
                                                        ( Label
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
                                                                `TArrow` ( TApplication
                                                                            KType
                                                                            (TConstructor (KArrow KType KType) "Tree")
                                                                            (TVariable (TypeIndex KType 0) :| [])
                                                                         )
                                                            )
                                                            "g"
                                                        )
                                                    )
                                                    ( ERecord
                                                        ()
                                                        undefined
                                                        ( Map.fromList
                                                            [
                                                              ( "max"
                                                              , ESelect () (Label undefined "max") (EVariable () (Label undefined "range"))
                                                              )
                                                            ,
                                                              ( "min"
                                                              , EVariable () (Label undefined "p")
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
                                              undefined
                                              (EVariable () (Label undefined "g"))
                                              (EVariable () (Label undefined "range") :| [])
                                          )
                                      )
                                  )
                                  :| []
                              )
                              <| EClause
                                ()
                                (PListLiteral () undefined [])
                                ( CPlain
                                    ()
                                    []
                                    ( ELambda
                                        ()
                                        (PAny () undefined :| [])
                                        (EConstructor () (Label undefined "Leaf"))
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
                                      (PVariable () (Label undefined "$fold:1"))
                                      ( ELambda
                                          ()
                                          (PVariable () (Label undefined "$fold:1:expr") :| [])
                                          ( EMatch
                                              ()
                                              undefined
                                              (EVariable () (Label undefined "$fold:1:expr"))
                                              ( EClause
                                                  ()
                                                  (PListCons () undefined (PVariable () (Label undefined "p")) (PVariable () (Label undefined "g")))
                                                  ( CPlain
                                                      ()
                                                      []
                                                      ( ELambda
                                                          ()
                                                          (PVariable () (Label undefined "range") :| [])
                                                          ( EIf
                                                              ()
                                                              undefined
                                                              ( EApplication
                                                                  ()
                                                                  undefined
                                                                  ( EBinaryOperator
                                                                      ()
                                                                      ( TVariable (TypeIndex KType 0)
                                                                          `TArrow` (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
                                                                          `TArrow` TIntrinsic IBool
                                                                      , OForwardApplication
                                                                      )
                                                                  )
                                                                  ( EVariable () (Label (TVariable (TypeIndex KType 0)) "p")
                                                                      :| [ EApplication
                                                                            ()
                                                                            (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
                                                                            ( EVariable
                                                                                ()
                                                                                ( Label
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
                                                                                                    (TVariable (TypeIndex KType 0))
                                                                                                    ( RExtend "min" (TVariable (TypeIndex KType 0)) RNil
                                                                                                    )
                                                                                                )
                                                                                            )
                                                                                        )
                                                                                    )
                                                                                    "range"
                                                                                )
                                                                                :| []
                                                                            )
                                                                         ]
                                                                  )
                                                              )
                                                              ( EApplication
                                                                  ()
                                                                  undefined
                                                                  (EConstructor () (Label undefined "Node"))
                                                                  ( EVariable () (Label undefined "p")
                                                                      <| EApplication
                                                                        ()
                                                                        undefined
                                                                        (EVariable () (Label undefined "$fold:1"))
                                                                        ( EVariable () (Label undefined "g")
                                                                            <| ERecord
                                                                              ()
                                                                              undefined
                                                                              ( Map.fromList
                                                                                  [ ("max", EVariable () (Label undefined "p"))
                                                                                  , ("min", ESelect () (Label undefined "min") (EVariable () (Label undefined "range")))
                                                                                  ]
                                                                              )
                                                                              Nothing
                                                                              :| []
                                                                        )
                                                                      <| EApplication
                                                                        ()
                                                                        undefined
                                                                        (EVariable () (Label undefined "$fold:1"))
                                                                        ( EVariable () (Label undefined "g")
                                                                            <| ERecord
                                                                              ()
                                                                              undefined
                                                                              ( Map.fromList
                                                                                  [ ("max", ESelect () (Label undefined "max") (EVariable () (Label undefined "range")))
                                                                                  , ("min", EVariable () (Label undefined "p"))
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
                                                                  undefined
                                                                  (EVariable () (Label undefined "$fold:1"))
                                                                  (EVariable () (Label undefined "g") <| EVariable () (Label undefined "range") :| [])
                                                              )
                                                          )
                                                      )
                                                      :| []
                                                  )
                                                  <| EClause
                                                    ()
                                                    (PListLiteral () undefined [])
                                                    ( CPlain
                                                        ()
                                                        []
                                                        (ELambda () (PAny () undefined :| []) (EConstructor () (Label undefined "Leaf")))
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
                                      undefined
                                      (EVariable () (Label undefined "$fold:1"))
                                      ( EVariable () (Label undefined "list")
                                          <| ERecord
                                            ()
                                            undefined
                                            ( Map.fromList
                                                [
                                                  ( "max"
                                                  , EApplication
                                                      ()
                                                      undefined
                                                      (EVariable () (Label undefined "from_int32"))
                                                      (ELiteral () (LInt32 (-1)) :| [])
                                                  )
                                                ,
                                                  ( "min"
                                                  , EApplication
                                                      ()
                                                      undefined
                                                      (EVariable () (Label undefined "from_int32"))
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
              (EVariable () (Label undefined "from_list"))
           )

runTest :: (Show a, Eq a) => Expression a () -> TestResult a
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
                                                    , ESelect () (Label () "min") (EVariable () (Label () "range"))
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
                                                    , ESelect () (Label () "max") (EVariable () (Label () "range"))
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
                    )
                )
            )
        )
        :| []
    )
    (EVariable () (Label () "from_list"))
