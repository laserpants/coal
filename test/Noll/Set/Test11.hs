{-# LANGUAGE OverloadedStrings #-}

module Noll.Set.Test11 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Module

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Noll.Module as Module

-- Dictionary elimination
prog1_11 :: [Module () Kind IndexedType]
prog1_11 =
  [ moduleUtils
  , moduleOrdered
  , moduleBinarySearch
  , moduleMain
  ]

moduleUtils :: Module () Kind IndexedType
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

moduleOrdered :: Module () Kind IndexedType
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
        [ DConstant
            "compare"
            ( Constant
                ()
                (With [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TConstructor KType "Ordering"))
                ( ELambda
                    ()
                    ( PVariable () (Label (TIntrinsic IInt32) "x")
                        <| PVariable () (Label (TIntrinsic IInt32) "y")
                        :| []
                    )
                    ( EIf
                        ()
                        (TConstructor KType "Ordering")
                        ( EApplication
                            ()
                            (TIntrinsic IBool)
                            ( EBinaryOperator
                                ()
                                (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IBool)
                                OLessThan
                            )
                            ( EVariable () (Label (TIntrinsic IInt32) "x")
                                <| EVariable () (Label (TIntrinsic IInt32) "y")
                                :| []
                            )
                        )
                        (EConstructor () (Label (TConstructor KType "Ordering") "LessThan"))
                        ( EIf
                            ()
                            (TConstructor KType "Ordering")
                            ( EApplication
                                ()
                                (TIntrinsic IBool)
                                ( EBinaryOperator
                                    ()
                                    (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IBool)
                                    OGreaterThan
                                )
                                ( EVariable () (Label (TIntrinsic IInt32) "x")
                                    <| EVariable () (Label (TIntrinsic IInt32) "y")
                                    :| []
                                )
                            )
                            (EConstructor () (Label (TConstructor KType "Ordering") "GreaterThan"))
                            (EConstructor () (Label (TConstructor KType "Ordering") "EqualTo"))
                        )
                    )
                )
            )
        ]
    , DAnnotation
        ( With
            [Trait "Ordered" (TVariable (Parameter () "a"))]
            ( TAlias
                "Predicate"
                [TVariable (Parameter () "a")]
                (TVariable (Parameter () "a") `TArrow` TIntrinsic IBool)
            )
        )
        ( DConstant
            "less_than_or_equal_to"
            ( Constant
                ()
                ( With
                    [Trait "Ordered" (TVariable (TypeIndex KType 0))]
                    (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
                )
                ( ELambda
                    ()
                    ( PVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 0) :| [])) "$dict.ffef54c635ab7d00")
                        <| PVariable () (Label (TVariable (TypeIndex KType 0)) "m")
                        <| PVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                        :| []
                    )
                    ( ECompiledMatch
                        ()
                        (TIntrinsic IBool)
                        ( EApplication
                            ()
                            (TConstructor KType "Ordering")
                            (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TConstructor KType "Ordering") "compare"))
                            ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 0) :| [])) "$dict.ffef54c635ab7d00")
                                <| EVariable () (Label (TVariable (TypeIndex KType 0)) "m")
                                <| EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                                :| []
                            )
                        )
                        ( ECompiledClause
                            (Label (TConstructor KType "Ordering") "EqualTo" :| [])
                            (ELiteral () (LBool True))
                            <| ECompiledClause
                              (Label (TConstructor KType "Ordering") "GreaterThan" :| [])
                              (ELiteral () (LBool False))
                            <| ECompiledClause
                              (Label (TConstructor KType "Ordering") "LessThan" :| [])
                              (ELiteral () (LBool True))
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
        ( DConstant
            "greater_than"
            ( Constant
                ()
                ( With
                    [Trait "Ordered" (TVariable (TypeIndex KType 1))]
                    (TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool)
                )
                ( ELambda
                    ()
                    ( PVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 1) :| [])) "$dict.ffef54c635ab7d01")
                        <| PAnnotation
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
                              (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 1) :| []) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool) "less_than_or_equal_to"))
                              ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 1) :| [])) "$dict.ffef54c635ab7d01")
                                  <| EVariable () (Label (TVariable (TypeIndex KType 1)) "n")
                                  :| []
                              )
                            :| []
                        )
                    )
                )
            )
        )
    ]

moduleBinarySearch :: Module () Kind IndexedType
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
        ( DConstant
            "in_range"
            ( Constant
                ()
                ( With
                    [ Trait "Numeric" (TVariable (TypeIndex KType 0))
                    , Trait "Ordered" (TVariable (TypeIndex KType 0))
                    ]
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
                        `TArrow` TVariable (TypeIndex KType 0)
                        `TArrow` TIntrinsic IBool
                    )
                )
                ( ELambda
                    ()
                    ( PVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 0) :| [])) "$dict.be194a5d16952b76")
                        <| PVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 0) :| [])) "$dict.ffef54c635ab7d00")
                        <| PVariable
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
                              "$v.0"
                          )
                        <| PAnnotation
                          ()
                          (TVariable (Parameter () "a"))
                          (PVariable () (Label (TVariable (TypeIndex KType 0)) "n"))
                        :| []
                    )
                    ( ECompiledMatch
                        ()
                        (TIntrinsic IBool)
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
                                "$v.0"
                            )
                        )
                        ( ECompiledClause
                            ( Label
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
                                    `TArrow` TIntrinsic
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
                                "$Record"
                                <| Label
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
                                  "$match.8.$row.1"
                                :| []
                            )
                            ( EFocus
                                "max"
                                (Label (TVariable (TypeIndex KType 0)) "$row.1.field.max")
                                (Label (TIntrinsic (IRecord (TRow (RExtend "min" (TVariable (TypeIndex KType 0)) RNil)))) "$row.1.tail")
                                ( EVariable
                                    ()
                                    ( Label
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
                                        "$match.8.$row.1"
                                    )
                                )
                                ( ECompiledMatch
                                    ()
                                    (TIntrinsic IBool)
                                    (EVariable () (Label (TIntrinsic (IRecord (TRow (RExtend "min" (TVariable (TypeIndex KType 0)) RNil)))) "$row.1.tail"))
                                    ( ECompiledClause
                                        ( Label
                                            ( TRow
                                                ( RExtend
                                                    "min"
                                                    (TVariable (TypeIndex KType 0))
                                                    RNil
                                                )
                                                `TArrow` TIntrinsic
                                                  ( IRecord
                                                      ( TRow
                                                          ( RExtend
                                                              "min"
                                                              (TVariable (TypeIndex KType 0))
                                                              RNil
                                                          )
                                                      )
                                                  )
                                            )
                                            "$Record"
                                            <| Label
                                              ( TRow
                                                  ( RExtend
                                                      "min"
                                                      (TVariable (TypeIndex KType 0))
                                                      RNil
                                                  )
                                              )
                                              "$match.5.$row.2"
                                            :| []
                                        )
                                        ( EFocus
                                            "min"
                                            (Label (TVariable (TypeIndex KType 0)) "$row.2.field.min")
                                            (Label (TIntrinsic (IRecord (TRow RNil))) "$row.2.tail")
                                            ( EVariable
                                                ()
                                                ( Label
                                                    ( TRow
                                                        ( RExtend
                                                            "min"
                                                            (TVariable (TypeIndex KType 0))
                                                            RNil
                                                        )
                                                    )
                                                    "$match.5.$row.2"
                                                )
                                            )
                                            ( ECompiledMatch
                                                ()
                                                (TIntrinsic IBool)
                                                (EVariable () (Label (TIntrinsic (IRecord (TRow RNil))) "$row.2.tail"))
                                                ( ECompiledClause
                                                    ( Label (TRow RNil `TArrow` TIntrinsic (IRecord (TRow RNil))) "$Record"
                                                        <| Label (TRow RNil) "$match.2._"
                                                        :| []
                                                    )
                                                    ( EApplication
                                                        ()
                                                        (TIntrinsic IBool)
                                                        (EBinaryOperator () (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) OLogicalAnd)
                                                        ( EApplication
                                                            ()
                                                            (TIntrinsic IBool)
                                                            (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "greater_than"))
                                                            ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 0) :| [])) "$dict.ffef54c635ab7d00")
                                                                <| EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                                                                <| EVariable () (Label (TVariable (TypeIndex KType 0)) "$row.2.field.min")
                                                                :| []
                                                            )
                                                            <| EApplication
                                                              ()
                                                              (TIntrinsic IBool)
                                                              (EBinaryOperator () (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) OLogicalOr)
                                                              ( EApplication
                                                                  ()
                                                                  (TIntrinsic IBool)
                                                                  (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "less_than_or_equal_to"))
                                                                  ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 0) :| [])) "$dict.ffef54c635ab7d00")
                                                                      <| EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                                                                      <| EVariable () (Label (TVariable (TypeIndex KType 0)) "$row.1.field.max")
                                                                      :| []
                                                                  )
                                                                  <| EApplication
                                                                    ()
                                                                    (TIntrinsic IBool)
                                                                    (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "less_than_or_equal_to"))
                                                                    ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 0) :| [])) "$dict.ffef54c635ab7d00")
                                                                        <| EVariable () (Label (TVariable (TypeIndex KType 0)) "$row.1.field.max")
                                                                        <| EApplication
                                                                          ()
                                                                          (TVariable (TypeIndex KType 0))
                                                                          (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 0) :| []) `TArrow` TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0)) "from_int32"))
                                                                          ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 0) :| [])) "$dict.be194a5d16952b76")
                                                                              <| ELiteral () (LInt32 (-1))
                                                                              :| []
                                                                          )
                                                                        :| []
                                                                    )
                                                                  :| []
                                                              )
                                                            :| []
                                                        )
                                                    )
                                                    :| []
                                                )
                                            )
                                        )
                                        :| []
                                    )
                                )
                            )
                            :| []
                        )
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
        ( DConstant
            "from_list"
            ( Constant
                ()
                ( With
                    [ Trait "Numeric" (TVariable (TypeIndex KType 1))
                    , Trait "Ordered" (TVariable (TypeIndex KType 1))
                    ]
                    (TIntrinsic (IList (TVariable (TypeIndex KType 1))) `TArrow` TApplication KType (TConstructor (KArrow KType KType) "Tree") (TVariable (TypeIndex KType 1) :| []))
                )
                ( ELambda
                    ()
                    ( PVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 1) :| [])) "$dict.be194a5d16952b77")
                        <| PVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 1) :| [])) "$dict.ffef54c635ab7d01")
                        <| PAnnotation
                          ()
                          (TIntrinsic (IList (TVariable (Parameter () "a"))))
                          (PVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "list"))
                        :| []
                    )
                    ( EFold
                        ()
                        ( TApplication
                            KType
                            (TConstructor (KArrow KType KType) "Tree")
                            (TVariable (TypeIndex KType 1) :| [])
                        )
                        ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "list")
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
                                    ( "min"
                                    , EApplication
                                        ()
                                        (TVariable (TypeIndex KType 1))
                                        (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 1) :| []) `TArrow` TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1)) "from_int32"))
                                        ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 1) :| [])) "$dict.be194a5d16952b77")
                                            <| ELiteral () (LInt32 0)
                                            :| []
                                        )
                                    )
                                  ,
                                    ( "max"
                                    , EApplication
                                        ()
                                        (TVariable (TypeIndex KType 1))
                                        (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 1) :| []) `TArrow` TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1)) "from_int32"))
                                        ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 1) :| [])) "$dict.be194a5d16952b77")
                                            <| ELiteral () (LInt32 (-1))
                                            :| []
                                        )
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
                                (TIntrinsic (IList (TVariable (TypeIndex KType 1))))
                                (PVariable () (Label (TVariable (TypeIndex KType 1)) "p"))
                                (PAtVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "g"))
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
                                                          ( TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 1) :| [])
                                                              `TArrow` TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 1) :| [])
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
                                                              `TArrow` TVariable (TypeIndex KType 1)
                                                              `TArrow` TIntrinsic IBool
                                                          )
                                                          "in_range"
                                                      )
                                                  )
                                                  ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 1) :| [])) "$dict.be194a5d16952b77")
                                                      <| EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 1) :| [])) "$dict.ffef54c635ab7d01")
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
                                                                      (TVariable (TypeIndex KType 1))
                                                                      (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                                  )
                                                              )
                                                          )
                                                      )
                                                      ( Map.fromList
                                                          [
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
                                                          ,
                                                            ( "max"
                                                            , EVariable () (Label (TVariable (TypeIndex KType 1)) "p")
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
                                                      (TVariable (TypeIndex KType 1) :| [])
                                                  )
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
                                                              `TArrow` ( TApplication
                                                                          KType
                                                                          (TConstructor (KArrow KType KType) "Tree")
                                                                          (TVariable (TypeIndex KType 1) :| [])
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
                                                                      (TVariable (TypeIndex KType 1))
                                                                      (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                                  )
                                                              )
                                                          )
                                                      )
                                                      ( Map.fromList
                                                          [
                                                            ( "min"
                                                            , EVariable () (Label (TVariable (TypeIndex KType 1)) "p")
                                                            )
                                                          ,
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
                                                        `TArrow` ( TApplication
                                                                    KType
                                                                    (TConstructor (KArrow KType KType) "Tree")
                                                                    (TVariable (TypeIndex KType 1) :| [])
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
                        ( Just
                            ( ELet
                                ()
                                ( BPattern
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
                                        ( ECompiledMatch
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
                                            ( ECompiledClause
                                                ( Label
                                                    ( TVariable (TypeIndex KType 1)
                                                        `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 1)))
                                                        `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 1)))
                                                    )
                                                    "$Cons"
                                                    <| Label (TVariable (TypeIndex KType 1)) "$match.10.p"
                                                    <| Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "$match.11.g"
                                                    :| []
                                                )
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
                                                            ( EVariable () (Label (TVariable (TypeIndex KType 1)) "$match.10.p")
                                                                <| EApplication
                                                                  ()
                                                                  (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool)
                                                                  ( EVariable
                                                                      ()
                                                                      ( Label
                                                                          ( TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 1) :| [])
                                                                              `TArrow` TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 1) :| [])
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
                                                                              `TArrow` TVariable (TypeIndex KType 1)
                                                                              `TArrow` TIntrinsic IBool
                                                                          )
                                                                          "in_range"
                                                                      )
                                                                  )
                                                                  ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 1) :| [])) "$dict.be194a5d16952b77")
                                                                      <| EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 1) :| [])) "$dict.ffef54c635ab7d01")
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
                                                            ( EVariable () (Label (TVariable (TypeIndex KType 1)) "$match.10.p")
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
                                                                  ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "$match.11.g")
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
                                                                                  , EVariable () (Label (TVariable (TypeIndex KType 1)) "$match.10.p")
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
                                                                  ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "$match.11.g")
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
                                                                              , EVariable () (Label (TVariable (TypeIndex KType 1)) "$match.10.p")
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
                                                            ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "$match.11.g")
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
                                                <| ECompiledClause
                                                  (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "$Nil" :| [])
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
                                        )
                                    )
                                    :| []
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
                                    ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "list")
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
                                                , EApplication
                                                    ()
                                                    (TVariable (TypeIndex KType 1))
                                                    (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 1) :| []) `TArrow` TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1)) "from_int32"))
                                                    ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 1) :| [])) "$dict.be194a5d16952b77")
                                                        <| ELiteral () (LInt32 (-1))
                                                        :| []
                                                    )
                                                )
                                              ,
                                                ( "min"
                                                , EApplication
                                                    ()
                                                    (TVariable (TypeIndex KType 1))
                                                    (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 1) :| []) `TArrow` TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1)) "from_int32"))
                                                    ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 1) :| [])) "$dict.be194a5d16952b77")
                                                        <| ELiteral () (LInt32 0)
                                                        :| []
                                                    )
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
        )
    , -- flatten
      DAnnotation
        (With [] (TIntrinsic (IList (TVariable (Parameter () "a")))))
        ( DConstant
            "flatten"
            ( Constant
                ()
                ( With
                    []
                    ( TApplication
                        KType
                        (TConstructor (KArrow KType KType) "Tree")
                        (TVariable (TypeIndex KType 2) :| [])
                        `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 2)))
                    )
                )
                ( ELambda
                    ()
                    ( PAnnotation
                        ()
                        (TApplication () (TConstructor () "Tree") (TVariable (Parameter () "a") :| []))
                        ( PVariable
                            ()
                            ( Label
                                ( TApplication
                                    KType
                                    (TConstructor (KArrow KType KType) "Tree")
                                    (TVariable (TypeIndex KType 2) :| [])
                                )
                                "tree"
                            )
                        )
                        :| []
                    )
                    ( EFold
                        ()
                        (TIntrinsic (IList (TVariable (TypeIndex KType 2))))
                        ( EVariable
                            ()
                            ( Label
                                ( TApplication
                                    KType
                                    (TConstructor (KArrow KType KType) "Tree")
                                    (TVariable (TypeIndex KType 2) :| [])
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
                                        (TVariable (TypeIndex KType 2) :| [])
                                    )
                                    "Node"
                                )
                                [ PVariable () (Label (TVariable (TypeIndex KType 2)) "y")
                                , PAtVariable
                                    ()
                                    ( Label
                                        ( TApplication
                                            KType
                                            (TConstructor (KArrow KType KType) "Tree")
                                            (TVariable (TypeIndex KType 2) :| [])
                                        )
                                        "lhs"
                                    )
                                , PAtVariable
                                    ()
                                    ( Label
                                        ( TApplication
                                            KType
                                            (TConstructor (KArrow KType KType) "Tree")
                                            (TVariable (TypeIndex KType 2) :| [])
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
                                    (TIntrinsic (IList (TVariable (TypeIndex KType 2))))
                                    ( EBinaryOperator
                                        ()
                                        ( TIntrinsic (IList (TVariable (TypeIndex KType 2)))
                                            `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 2)))
                                            `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 2)))
                                        )
                                        OListConcatenation
                                    )
                                    ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 2)))) "lhs")
                                        <| EListCons
                                          ()
                                          (TIntrinsic (IList (TVariable (TypeIndex KType 2))))
                                          (EVariable () (Label (TVariable (TypeIndex KType 2)) "y"))
                                          (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 2)))) "rhs"))
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
                                          (TVariable (TypeIndex KType 2) :| [])
                                      )
                                      "Leaf"
                                  )
                                  []
                              )
                              ( CPlain
                                  ()
                                  []
                                  (EListLiteral () (TIntrinsic (IList (TVariable (TypeIndex KType 2)))) [])
                                  :| []
                              )
                            :| []
                        )
                        ( Just
                            ( ELet
                                ()
                                ( BPattern
                                    ()
                                    ( PVariable
                                        ()
                                        ( Label
                                            ( ( TApplication
                                                  KType
                                                  (TConstructor (KArrow KType KType) "Tree")
                                                  (TVariable (TypeIndex KType 2) :| [])
                                              )
                                                `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 2)))
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
                                                    (TVariable (TypeIndex KType 2) :| [])
                                                )
                                                "$fold.2.expr"
                                            )
                                            :| []
                                        )
                                        ( ECompiledMatch
                                            ()
                                            (TIntrinsic (IList (TVariable (TypeIndex KType 2))))
                                            ( EVariable
                                                ()
                                                ( Label
                                                    ( TApplication
                                                        KType
                                                        (TConstructor (KArrow KType KType) "Tree")
                                                        (TVariable (TypeIndex KType 2) :| [])
                                                    )
                                                    "$fold.2.expr"
                                                )
                                            )
                                            ( ECompiledClause
                                                ( Label
                                                    ( TApplication
                                                        KType
                                                        (TConstructor (KArrow KType KType) "Tree")
                                                        (TVariable (TypeIndex KType 2) :| [])
                                                    )
                                                    "Leaf"
                                                    :| []
                                                )
                                                (EListLiteral () (TIntrinsic (IList (TVariable (TypeIndex KType 2)))) [])
                                                <| ECompiledClause
                                                  ( Label
                                                      ( TVariable (TypeIndex KType 2)
                                                          `TArrow` TApplication
                                                            KType
                                                            (TConstructor (KArrow KType KType) "Tree")
                                                            (TVariable (TypeIndex KType 2) :| [])
                                                          `TArrow` TApplication
                                                            KType
                                                            (TConstructor (KArrow KType KType) "Tree")
                                                            (TVariable (TypeIndex KType 2) :| [])
                                                          `TArrow` TApplication
                                                            KType
                                                            (TConstructor (KArrow KType KType) "Tree")
                                                            (TVariable (TypeIndex KType 2) :| [])
                                                      )
                                                      "Node"
                                                      <| Label (TVariable (TypeIndex KType 2)) "$match.13.y"
                                                      <| Label
                                                        ( TApplication
                                                            KType
                                                            (TConstructor (KArrow KType KType) "Tree")
                                                            (TVariable (TypeIndex KType 2) :| [])
                                                        )
                                                        "$match.14.lhs"
                                                      <| Label
                                                        ( TApplication
                                                            KType
                                                            (TConstructor (KArrow KType KType) "Tree")
                                                            (TVariable (TypeIndex KType 2) :| [])
                                                        )
                                                        "$match.15.rhs"
                                                      :| []
                                                  )
                                                  ( EApplication
                                                      ()
                                                      (TIntrinsic (IList (TVariable (TypeIndex KType 2))))
                                                      ( EBinaryOperator
                                                          ()
                                                          ( TIntrinsic (IList (TVariable (TypeIndex KType 2)))
                                                              `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 2)))
                                                              `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 2)))
                                                          )
                                                          OListConcatenation
                                                      )
                                                      ( EApplication
                                                          ()
                                                          (TIntrinsic (IList (TVariable (TypeIndex KType 2))))
                                                          ( EVariable
                                                              ()
                                                              ( Label
                                                                  ( ( TApplication
                                                                        KType
                                                                        (TConstructor (KArrow KType KType) "Tree")
                                                                        (TVariable (TypeIndex KType 2) :| [])
                                                                    )
                                                                      `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 2)))
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
                                                                      (TVariable (TypeIndex KType 2) :| [])
                                                                  )
                                                                  "$match.14.lhs"
                                                              )
                                                              :| []
                                                          )
                                                          <| EListCons
                                                            ()
                                                            (TIntrinsic (IList (TVariable (TypeIndex KType 2))))
                                                            (EVariable () (Label (TVariable (TypeIndex KType 2)) "$match.13.y"))
                                                            ( EApplication
                                                                ()
                                                                (TIntrinsic (IList (TVariable (TypeIndex KType 2))))
                                                                ( EVariable
                                                                    ()
                                                                    ( Label
                                                                        ( ( TApplication
                                                                              KType
                                                                              (TConstructor (KArrow KType KType) "Tree")
                                                                              (TVariable (TypeIndex KType 2) :| [])
                                                                          )
                                                                            `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 2)))
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
                                                                            (TVariable (TypeIndex KType 2) :| [])
                                                                        )
                                                                        "$match.15.rhs"
                                                                    )
                                                                    :| []
                                                                )
                                                            )
                                                          :| []
                                                      )
                                                  )
                                                :| []
                                            )
                                        )
                                    )
                                    :| []
                                )
                                ( EApplication
                                    ()
                                    (TIntrinsic (IList (TVariable (TypeIndex KType 2))))
                                    ( EVariable
                                        ()
                                        ( Label
                                            ( ( TApplication
                                                  KType
                                                  (TConstructor (KArrow KType KType) "Tree")
                                                  (TVariable (TypeIndex KType 2) :| [])
                                              )
                                                `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 2)))
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
                                                (TVariable (TypeIndex KType 2) :| [])
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
                ( With
                    [ Trait "Numeric" (TVariable (TypeIndex KType 3))
                    , Trait "Ordered" (TVariable (TypeIndex KType 3))
                    ]
                    ( TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                        `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                    )
                )
                ( ELambda
                    ()
                    ( PVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 3) :| [])) "$dict.be194a5d16952b75")
                        <| PVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 3) :| [])) "$dict.ffef54c635ab7d03")
                        :| []
                    )
                    ( EApplication
                        ()
                        ( TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                            `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                        )
                        ( EBinaryOperator
                            ()
                            ( ( TApplication
                                  KType
                                  (TConstructor (KArrow KType KType) "Tree")
                                  (TVariable (TypeIndex KType 3) :| [])
                                  `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                              )
                                `TArrow` ( TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                                            `TArrow` TApplication
                                              KType
                                              (TConstructor (KArrow KType KType) "Tree")
                                              (TVariable (TypeIndex KType 3) :| [])
                                         )
                                `TArrow` ( (TIntrinsic (IList (TVariable (TypeIndex KType 3))))
                                            `TArrow` (TIntrinsic (IList (TVariable (TypeIndex KType 3))))
                                         )
                            )
                            OReverseComposition
                        )
                        ( EVariable
                            ()
                            ( Label
                                ( TApplication
                                    KType
                                    (TConstructor (KArrow KType KType) "Tree")
                                    (TVariable (TypeIndex KType 3) :| [])
                                    `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                                )
                                "flatten"
                            )
                            <| EApplication
                              ()
                              ( TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                                  `TArrow` TApplication
                                    KType
                                    (TConstructor (KArrow KType KType) "Tree")
                                    (TVariable (TypeIndex KType 3) :| [])
                              )
                              ( EVariable
                                  ()
                                  ( Label
                                      ( TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 3) :| [])
                                          `TArrow` TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 3) :| [])
                                          `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                                          `TArrow` TApplication KType (TConstructor (KArrow KType KType) "Tree") (TVariable (TypeIndex KType 3) :| [])
                                      )
                                      "from_list"
                                  )
                              )
                              ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 3) :| [])) "$dict.be194a5d16952b75")
                                  <| EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 3) :| [])) "$dict.ffef54c635ab7d03")
                                  :| []
                              )
                            :| []
                        )
                    )
                )
            )
        )
    ]

moduleMain :: Module () Kind IndexedType
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
    , -- main
      DConstant
        "main"
        ( Constant
            ()
            ( With
                []
                (TIntrinsic IUnit `TArrow` TVariable (TypeIndex KType 0))
            )
            ( ELambda
                ()
                (PLiteral () LUnit :| [])
                ( ELet
                    ()
                    ( BPattern
                        ()
                        (PVariable () (Label (TIntrinsic (IList (TIntrinsic IInt32))) "xs"))
                        ( EAnnotation
                            ()
                            (TIntrinsic (IList (TIntrinsic IInt32)))
                            ( EListLiteral
                                ()
                                (TIntrinsic (IList (TIntrinsic IInt32)))
                                [ EApplication
                                    ()
                                    (TIntrinsic IInt32)
                                    (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| []) `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "from_int32"))
                                    ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| [])) "$dict.2967b53e939a3c94")
                                        <| ELiteral () (LInt32 5)
                                        :| []
                                    )
                                , EApplication
                                    ()
                                    (TIntrinsic IInt32)
                                    (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| []) `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "from_int32"))
                                    ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| [])) "$dict.2967b53e939a3c94")
                                        <| ELiteral () (LInt32 3)
                                        :| []
                                    )
                                , EApplication
                                    ()
                                    (TIntrinsic IInt32)
                                    (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| []) `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "from_int32"))
                                    ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| [])) "$dict.2967b53e939a3c94")
                                        <| ELiteral () (LInt32 7)
                                        :| []
                                    )
                                , EApplication
                                    ()
                                    (TIntrinsic IInt32)
                                    (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| []) `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "from_int32"))
                                    ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| [])) "$dict.2967b53e939a3c94")
                                        <| ELiteral () (LInt32 2)
                                        :| []
                                    )
                                , EApplication
                                    ()
                                    (TIntrinsic IInt32)
                                    (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| []) `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "from_int32"))
                                    ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| [])) "$dict.2967b53e939a3c94")
                                        <| ELiteral () (LInt32 1)
                                        :| []
                                    )
                                , EApplication
                                    ()
                                    (TIntrinsic IInt32)
                                    (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| []) `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "from_int32"))
                                    ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| [])) "$dict.2967b53e939a3c94")
                                        <| ELiteral () (LInt32 6)
                                        :| []
                                    )
                                , EApplication
                                    ()
                                    (TIntrinsic IInt32)
                                    (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| []) `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "from_int32"))
                                    ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| [])) "$dict.2967b53e939a3c94")
                                        <| ELiteral () (LInt32 4)
                                        :| []
                                    )
                                ]
                            )
                        )
                        :| []
                    )
                    ( EApplication
                        ()
                        (TVariable (TypeIndex KType 0))
                        (EVariable () (Label (TIntrinsic (IList (TIntrinsic IInt32)) `TArrow` TVariable (TypeIndex KType 0)) "trace"))
                        ( EApplication
                            ()
                            (TIntrinsic (IList (TIntrinsic IInt32)))
                            (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| []) `TArrow` TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TIntrinsic IInt32 :| []) `TArrow` TIntrinsic (IList (TIntrinsic IInt32)) `TArrow` TIntrinsic (IList (TIntrinsic IInt32))) "sort"))
                            ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| [])) "$dict.2967b53e939a3c94")
                                <| EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TIntrinsic IInt32 :| [])) "$dict.b7c5e7e84eeaf782")
                                <| EVariable () (Label (TIntrinsic (IList (TIntrinsic IInt32))) "xs")
                                :| []
                            )
                            :| []
                        )
                    )
                )
            )
        )
    ]
