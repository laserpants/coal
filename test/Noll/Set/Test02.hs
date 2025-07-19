{-# LANGUAGE OverloadedStrings #-}

module Noll.Set.Test02 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..), Path (..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Noll.Module as Module

-- Expand type aliases
prog1_02 :: [Module () () ()]
prog1_02 =
  [ moduleUtils
  , moduleOrdered
  , moduleBinarySearch
  , moduleMain
  ]

moduleUtils :: Module () () ()
moduleUtils =
  Module.fromDefinitionList
    (Path ["Utils"])
    -- Exports
    ["Predicate"]
    -- Definitions
    [ -- type_alias Predicate
      DTypeAlias
        "Predicate"
        [Parameter () "a"]
        (TVariable (Parameter () "a") `TArrow` TIntrinsic IBool)
    ]

moduleOrdered :: Module () () ()
moduleOrdered =
  Module.fromDefinitionList
    (Path ["Ordered"])
    -- Exports
    ["Ordering", "Ordered", "less_than_or_equal_to", "greater_than"]
    -- Definitions
    [ -- import Utils(Predicate)
      DImport (Path ["Utils"]) ["Predicate"]
    , -- type Ordering
      DType
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
    , -- trait Ordered
      DTrait
        "Ordered"
        []
        (TVariable (Parameter () "a"))
        [
          ( "compare"
          , TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TConstructor () "Ordering"
          )
        ]
    , -- instance Ordered(int32)
      DInstance
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
    , -- less_than_or_equal_to
      DAnnotation
        ( With
            [Trait "Ordered" (TVariable (Parameter () "a"))]
            ( TAlias
                "Predicate"
                [TVariable (Parameter () "a")]
                (TVariable (Parameter () "a") `TArrow` TIntrinsic IBool)
            )
        )
        ( DFunction
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
    , -- greater_than
      DAnnotation
        ( With
            [Trait "Ordered" (TVariable (Parameter () "a"))]
            ( TAlias
                "Predicate"
                [TVariable (Parameter () "a")]
                (TVariable (Parameter () "a") `TArrow` TIntrinsic IBool)
            )
        )
        ( DFunction
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

moduleBinarySearch :: Module () () ()
moduleBinarySearch =
  Module.fromDefinitionList
    (Path ["BinarySearch"])
    -- Exports
    ["Tree", "from_list", "flatten"]
    -- Definitions
    [ -- import Ordered(Ordering, Ordered, less_than_or_equal_to, greater_than)
      DImport
        (Path ["Ordered"])
        ["LessThan", "EqualTo", "GreaterThan", "compare", "less_than_or_equal_to", "greater_than"]
    , -- type Tree
      DType
        "Tree"
        [Parameter () "a"]
        [ Constructor
            "Node"
            3
            ( Forall
                (Set.fromList [Parameter () "a"])
                []
                ( TVariable (Parameter () "a")
                    `TArrow` (TApplication () (TConstructor () "Tree") (TVariable (Parameter () "a") :| []))
                    `TArrow` (TApplication () (TConstructor () "Tree") (TVariable (Parameter () "a") :| []))
                    `TArrow` (TApplication () (TConstructor () "Tree") (TVariable (Parameter () "a") :| []))
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
    , -- trait Numeric
      DTrait
        "Numeric"
        []
        (TVariable (Parameter () "a"))
        [
          ( "from_int32"
          , TIntrinsic IInt32 `TArrow` TVariable (Parameter () "a")
          )
        ]
    , -- type_alias Range
      DTypeAlias
        "Range"
        [Parameter () "a"]
        ( TIntrinsic
            ( IRecord
                ( TRow
                    ( RExtend
                        "min"
                        (TVariable (Parameter () "a"))
                        (RExtend "max" (TVariable (Parameter () "a")) RNil)
                    )
                )
            )
        )
    , -- in_range
      DAnnotation
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
                    ( PRecord
                        ()
                        ()
                        ( Map.fromList
                            [
                              ( "min"
                              , PVariable () (Label () "min")
                              )
                            ,
                              ( "max"
                              , PVariable () (Label () "max")
                              )
                            ]
                        )
                        Nothing
                    )
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
                            <| EVariable () (Label () "min")
                            :| []
                        )
                        <| EApplication
                          ()
                          ()
                          (EBinaryOperator () () OLogicalOr)
                          ( EApplication
                              ()
                              ()
                              (EVariable () (Label () "less_than_or_equal_to"))
                              ( EVariable () (Label () "n")
                                  <| EVariable () (Label () "max")
                                  :| []
                              )
                              <| EApplication
                                ()
                                ()
                                (EVariable () (Label () "less_than_or_equal_to"))
                                ( EVariable () (Label () "max")
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
    , -- from_list
      DAnnotation
        ( With
            [ Trait "Ordered" (TVariable (Parameter () "a"))
            , Trait "Numeric" (TVariable (Parameter () "a"))
            ]
            (TApplication () (TConstructor () "Tree") (TVariable (Parameter () "a") :| []))
        )
        ( DFunction
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
                    Nothing
                )
            )
        )
    , -- flatten
      DAnnotation
        (With [] (TIntrinsic (IList (TVariable (Parameter () "a")))))
        ( DFunction
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
                    Nothing
                )
            )
        )
    , -- sort
      DAnnotation
        ( With
            [ Trait "Ordered" (TVariable (Parameter () "a"))
            , Trait "Numeric" (TVariable (Parameter () "a"))
            ]
            (TIntrinsic (IList (TVariable (Parameter () "a"))) `TArrow` TIntrinsic (IList (TVariable (Parameter () "a"))))
        )
        ( DConstant
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

moduleMain :: Module () () ()
moduleMain =
  Module.fromDefinitionList
    (Path ["Main"])
    -- Exports
    []
    -- Definitions
    [ -- import BinarySearch
      DImport
        (Path ["BinarySearch"])
        ["Node", "Leaf", "sort", "in_range", "from_int32"]
    , DImport
        (Path ["Ordered"])
        ["Ordered__$instance.b7c5e7e84eeaf782"]
    , DInstance
        "Numeric"
        (TIntrinsic IInt32)
        [ DFunction
            "from_int32"
            ( Function
                ()
                (With [] ())
                (PVariable () (Label () "x") :| [])
                (EVariable () (Label () "x"))
            )
        ]
    , -- main
      DFunction
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
                    ( EAnnotation
                        ()
                        (TIntrinsic (IList (TIntrinsic IInt32)))
                        ( EListLiteral
                            ()
                            ()
                            [ EApplication () () (EVariable () (Label () "from_int32")) (ELiteral () (LInt32 5) :| [])
                            , EApplication () () (EVariable () (Label () "from_int32")) (ELiteral () (LInt32 3) :| [])
                            , EApplication () () (EVariable () (Label () "from_int32")) (ELiteral () (LInt32 7) :| [])
                            , EApplication () () (EVariable () (Label () "from_int32")) (ELiteral () (LInt32 2) :| [])
                            , EApplication () () (EVariable () (Label () "from_int32")) (ELiteral () (LInt32 1) :| [])
                            , EApplication () () (EVariable () (Label () "from_int32")) (ELiteral () (LInt32 6) :| [])
                            , EApplication () () (EVariable () (Label () "from_int32")) (ELiteral () (LInt32 4) :| [])
                            ]
                        )
                    )
                    :| []
                )
                ( EApplication
                    ()
                    ()
                    (EVariable () (Label () "@@@_trace_int32"))
                    ( EMatch
                        ()
                        ()
                        ( EApplication
                            ()
                            ()
                            (EVariable () (Label () "sort"))
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
