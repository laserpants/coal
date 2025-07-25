{-# LANGUAGE OverloadedStrings #-}

module Noll.Set22.Test01 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Common.Label (Label (..))
import Noll.Language
import Noll.Language.Module (Constant (..), Definition (..), Function (..), Module (..), Path (..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Noll.Language.Module as Module

-- Untyped source tree
prog1_01 :: [Module () () ()]
prog1_01 =
  [ moduleMain
  ]

moduleMain :: Module () o ()
moduleMain =
  Module.fromDefinitionList
    (Path ["Main"])
    -- Exports
    []
    -- Definitions
    [ DImport (Path ["Core$"]) ["trace_int32", "trace_bool", "operator__not", "always"]
    , DType
        "Ordering"
        []
        [ Constructor
            "LessThan"
            0
            (Forall mempty [] (TConstructor () "Ordering"))
        , Constructor
            "EqualTo"
            0
            (Forall mempty [] (TConstructor () "Ordering"))
        , Constructor
            "GreaterThan"
            0
            (Forall mempty [] (TConstructor () "Ordering"))
        ]
    , DType
        "Tree"
        [Parameter () "a"]
        [ Constructor
            "Node"
            3
            ( Forall
                (Set.fromList [Parameter () "a"])
                []
                ( TVariable (Parameter () "a")
                    `TArrow` TApplication () (TConstructor () "Tree") (TVariable (Parameter () "a") :| [])
                    `TArrow` TApplication () (TConstructor () "Tree") (TVariable (Parameter () "a") :| [])
                    `TArrow` TApplication () (TConstructor () "Tree") (TVariable (Parameter () "a") :| [])
                )
            )
        , Constructor
            "Leaf"
            0
            ( Forall
                (Set.fromList [Parameter () "a"])
                []
                ( TApplication
                    ()
                    (TConstructor () "Tree")
                    (TVariable (Parameter () "a") :| [])
                )
            )
        ]
    , DFunction
        "f_compare"
        ( Function
            ()
            (With [] ())
            ( PAnnotation
                ()
                (TIntrinsic IInt32)
                (PVariable () (Label () "x"))
                <| PAnnotation
                  ()
                  (TIntrinsic IInt32)
                  (PVariable () (Label () "y"))
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
    , DFunction
        "f_less_than_or_equal_to"
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
                        (EVariable () (Label () "f_compare"))
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
    , DFunction
        "f_greater_than"
        ( Function
            ()
            (With [] ())
            (PVariable () (Label () "n") :| [])
            ( EApplication
                ()
                ()
                (EBinaryOperator () () OReverseComposition)
                ( EVariable () (Label () "operator__not")
                    <| EApplication
                      ()
                      ()
                      (EVariable () (Label () "f_less_than_or_equal_to"))
                      (EVariable () (Label () "n") :| [])
                    :| []
                )
            )
        )
    , DFunction
        "f_in_range"
        ( Function
            ()
            (With [] ())
            ( PVariable () (Label () "range")
                <| PVariable () (Label () "n")
                :| []
            )
            ( EApplication
                ()
                ()
                (EBinaryOperator () () OLogicalAnd)
                ( EApplication
                    ()
                    ()
                    (EVariable () (Label () "f_greater_than"))
                    ( EVariable () (Label () "n")
                        <| ESelect () (Label () "min") (EVariable () (Label () "range"))
                        :| []
                    )
                    <| EApplication
                      ()
                      ()
                      (EBinaryOperator () () OLogicalOr)
                      ( EApplication
                          ()
                          ()
                          (EVariable () (Label () "f_less_than_or_equal_to"))
                          ( EVariable () (Label () "n")
                              <| ESelect () (Label () "max") (EVariable () (Label () "range"))
                              :| []
                          )
                          <| EApplication
                            ()
                            ()
                            (EVariable () (Label () "f_less_than_or_equal_to"))
                            ( ESelect () (Label () "max") (EVariable () (Label () "range"))
                                <| ELiteral () (LInt32 (-1))
                                :| []
                            )
                          :| []
                      )
                    :| []
                )
            )
        )
    , DFunction
        "f_from_list"
        ( Function
            ()
            (With [] ())
            (PVariable () (Label () "list") :| [])
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
                            , ELiteral () (LInt32 0)
                            )
                          ,
                            ( "max"
                            , ELiteral () (LInt32 (-1))
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
                                          (EVariable () (Label () "f_in_range"))
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
                Nothing
            )
        )
    , DFunction
        "f_flatten"
        ( Function
            ()
            (With [] ())
            (PVariable () (Label () "tree") :| [])
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
                Nothing
            )
        )
    , DConstant
        "f_sort"
        ( Constant
            ()
            (With [] ())
            ( EApplication
                ()
                ()
                (EBinaryOperator () () OReverseComposition)
                ( EVariable () (Label () "f_flatten")
                    <| EVariable () (Label () "f_from_list")
                    :| []
                )
            )
        )
    , DFunction
        "main"
        ( Function
            ()
            (With [] ())
            (PLiteral () LUnit :| [])
            ( ELet
                ()
                ( BPattern
                    ()
                    (PVariable () (Label () "xs"))
                    ( EListLiteral
                        ()
                        ()
                        [ ELiteral () (LInt32 5)
                        , ELiteral () (LInt32 3)
                        , ELiteral () (LInt32 7)
                        , ELiteral () (LInt32 2)
                        , ELiteral () (LInt32 1)
                        , ELiteral () (LInt32 6)
                        , ELiteral () (LInt32 4)
                        ]
                    )
                    :| []
                )
                ( EApplication
                    ()
                    ()
                    (EVariable () (Label () "trace_int32"))
                    ( EMatch
                        ()
                        ()
                        ( EApplication
                            ()
                            ()
                            (EVariable () (Label () "f_sort"))
                            (EVariable () (Label () "xs") :| [])
                        )
                        ( EClause
                            ()
                            (PListLiteral () () [])
                            ( CPlain
                                ()
                                []
                                (ELiteral () (LInt32 12345))
                                :| []
                            )
                            <| EClause
                              ()
                              (PListCons () () (PVariable () (Label () "y")) (PAny () ()))
                              ( CPlain
                                  ()
                                  []
                                  (EVariable () (Label () "y"))
                                  :| []
                              )
                            :| []
                        )
                        :| []
                    )
                )
            )
        )
    ]
