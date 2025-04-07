{-# LANGUAGE OverloadedStrings #-}

module Noll.Set.Test01 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..), Path (..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Noll.Module as Module

prog1 :: [Module () () ()]
prog1 =
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
        [TVariable (Parameter () "a")]
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
        ,
          ( "from_int32"
          , TIntrinsic IInt32 `TArrow` TVariable (Parameter () "a")
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
                (Uses [] ())
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
        , DFunction
            "from_int32"
            ( Function
                ()
                (Uses [] ())
                (PVariable () (Label () "n") :| [])
                (EVariable () (Label () "n"))
            )
        ]
    , -- less_than_or_equal_to
      DAnnotation
        ( Uses
            [Trait "Ordered" (TVariable (Parameter () "a"))]
            ( TApplication
                ()
                (TConstructor () "Predicate")
                (TVariable (Parameter () "a") :| [])
            )
        )
        ( DFunction
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
        )
    , -- greater_than
      DAnnotation
        ( Uses
            [Trait "Ordered" (TVariable (Parameter () "a"))]
            ( TApplication
                ()
                (TConstructor () "Predicate")
                ( TVariable (Parameter () "a")
                    :| []
                )
            )
        )
        ( DFunction
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
      DImport (Path ["Ordered"]) ["Ordering", "Ordered", "less_than_or_equal_to", "greater_than"]
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
    , -- type_alias Range
      DTypeAlias
        "Range"
        [TVariable (Parameter () "a")]
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
        ( Uses
            [Trait "Ordered" (TVariable (Parameter () "a"))]
            (TIntrinsic IBool)
        )
        ( DFunction
            "in_range"
            ( Function
                ()
                (Uses [] ())
                ( PAnnotation
                    ()
                    ( TApplication
                        ()
                        (TConstructor () "Range")
                        (TVariable (Parameter () "a") :| [])
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
    , -- from_list
      DAnnotation
        ( Uses
            [Trait "Ordered" (TVariable (Parameter () "a"))]
            (TApplication () (TConstructor () "Tree") (TVariable (Parameter () "a") :| []))
        )
        ( DFunction
            "from_list"
            ( Function
                ()
                (Uses [] ())
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
                                        (EBinaryOperator () () OForwardApplication)
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
        (Uses [] (TIntrinsic (IList (TVariable (Parameter () "a")))))
        ( DFunction
            "flatten"
            ( Function
                ()
                (Uses [] ())
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
        ( Uses
            [Trait "Ordered" (TVariable (Parameter () "a"))]
            (TIntrinsic (IList (TVariable (Parameter () "a"))) `TArrow` TIntrinsic (IList (TVariable (Parameter () "a"))))
        )
        ( DConstant
            "qsort"
            ( Constant
                ()
                (Uses [] ())
                ( EApplication
                    ()
                    ()
                    (EBinaryOperator () () OForwardApplication)
                    (EVariable () (Label () "flatten") <| EVariable () (Label () "from_list") :| [])
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
      DImport (Path ["BinarySearch"]) ["sort"]
    , -- main
      DFunction
        "main"
        ( Function
            ()
            (Uses [] ())
            (PLiteral () LUnit :| [])
            ( EApplication
                ()
                ()
                (EVariable () (Label () "trace"))
                ( EApplication
                    ()
                    ()
                    (EVariable () (Label () "qsort"))
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
                        :| []
                    )
                    :| []
                )
            )
        )
    ]
