{-# LANGUAGE OverloadedStrings #-}

module Noll.Set.Test09 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..), Path (..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Noll.Module as Module

-- Compile match statements
prog1_09 :: [Module () Kind IndexedType]
prog1_09 =
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
                  (With [] (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool))
                  ( ELambda
                      ()
                      ( PVariable () (Label (TVariable (TypeIndex KType 0)) "m")
                          <| PVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                          :| []
                      )
                      ( ECompiledMatch
                          ()
                          (TIntrinsic IBool)
                          ( EApplication
                              ()
                              (TConstructor KType "Ordering")
                              (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TConstructor KType "Ordering") "compare"))
                              ( EVariable () (Label (TVariable (TypeIndex KType 0)) "m")
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
                (With [] (TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool))
                ( ELambda
                    ()
                    ( PAnnotation
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
                              (EVariable () (Label (TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool) "less_than_or_equal_to"))
                              (EVariable () (Label (TVariable (TypeIndex KType 1)) "n") :| [])
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
                    []
                    ( ( TIntrinsic
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
                        `TArrow` TVariable (TypeIndex KType 0)
                        `TArrow` TIntrinsic IBool
                    )
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
                                ( ( TRow
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
                                <| ( Label
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
                                                        <| (Label (TRow RNil) "$match.2._")
                                                        :| []
                                                    )
                                                    ( EApplication
                                                        ()
                                                        (TIntrinsic IBool)
                                                        (EBinaryOperator () (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) OLogicalAnd)
                                                        ( EApplication
                                                            ()
                                                            (TIntrinsic IBool)
                                                            (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "greater_than"))
                                                            ( EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                                                                <| EVariable () (Label (TVariable (TypeIndex KType 0)) "$row.2.field.min")
                                                                :| []
                                                            )
                                                            <| ( EApplication
                                                                  ()
                                                                  (TIntrinsic IBool)
                                                                  (EBinaryOperator () (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) OLogicalOr)
                                                               )
                                                              ( EApplication
                                                                  ()
                                                                  (TIntrinsic IBool)
                                                                  (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "less_than_or_equal_to"))
                                                                  ( EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                                                                      <| EVariable () (Label (TVariable (TypeIndex KType 0)) "$row.1.field.max")
                                                                      :| []
                                                                  )
                                                                  <| EApplication
                                                                    ()
                                                                    (TIntrinsic IBool)
                                                                    ( EBinaryOperator
                                                                        ()
                                                                        (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
                                                                        OEqualTo
                                                                    )
                                                                    ( EVariable () (Label (TVariable (TypeIndex KType 0)) "$row.1.field.max")
                                                                        <| EApplication
                                                                          ()
                                                                          (TVariable (TypeIndex KType 0))
                                                                          (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0)) "from_int32"))
                                                                          (ELiteral () (LInt32 (-1)) :| [])
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
                (With [] (TIntrinsic (IList (TVariable (TypeIndex KType 2))) `TArrow` TApplication KType (TConstructor (KArrow KType KType) "Tree") (TVariable (TypeIndex KType 2) :| [])))
                ( ELambda
                    ()
                    ( PAnnotation
                        ()
                        (TIntrinsic (IList (TVariable (Parameter () "a"))))
                        (PVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 2)))) "list"))
                        :| []
                    )
                    ( EFold
                        ()
                        ( TApplication
                            KType
                            (TConstructor (KArrow KType KType) "Tree")
                            (TVariable (TypeIndex KType 2) :| [])
                        )
                        ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 2)))) "list")
                            <| ERecord
                              ()
                              ( TIntrinsic
                                  ( IRecord
                                      ( TRow
                                          ( RExtend
                                              "max"
                                              (TVariable (TypeIndex KType 2))
                                              (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                          )
                                      )
                                  )
                              )
                              ( Map.fromList
                                  [
                                    ( "min"
                                    , EApplication
                                        ()
                                        (TVariable (TypeIndex KType 2))
                                        (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
                                        (ELiteral () (LInt32 0) :| [])
                                    )
                                  ,
                                    ( "max"
                                    , EApplication
                                        ()
                                        (TVariable (TypeIndex KType 2))
                                        (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
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
                                (TIntrinsic (IList (TVariable (TypeIndex KType 2))))
                                (PVariable () (Label (TVariable (TypeIndex KType 2)) "p"))
                                (PAtVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 2)))) "g"))
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
                                                            (TVariable (TypeIndex KType 2))
                                                            (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
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
                                            (TVariable (TypeIndex KType 2) :| [])
                                        )
                                        ( EApplication
                                            ()
                                            (TIntrinsic IBool)
                                            ( EBinaryOperator
                                                ()
                                                ( TArrow
                                                    (TVariable (TypeIndex KType 2))
                                                    (TArrow (TArrow (TVariable (TypeIndex KType 2)) (TIntrinsic IBool)) (TIntrinsic IBool))
                                                )
                                                OReverseApplication
                                            )
                                            ( EVariable () (Label (TVariable (TypeIndex KType 2)) "p")
                                                <| EApplication
                                                  ()
                                                  (TVariable (TypeIndex KType 2) `TArrow` TIntrinsic IBool)
                                                  ( EVariable
                                                      ()
                                                      ( Label
                                                          ( ( TIntrinsic
                                                                ( IRecord
                                                                    ( TRow
                                                                        ( RExtend
                                                                            "max"
                                                                            (TVariable (TypeIndex KType 2))
                                                                            (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                              `TArrow` TVariable (TypeIndex KType 2)
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
                                                                          (TVariable (TypeIndex KType 2))
                                                                          (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
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
                                                (TVariable (TypeIndex KType 2) :| [])
                                            )
                                            ( EConstructor
                                                ()
                                                ( Label
                                                    ( (TVariable (TypeIndex KType 2))
                                                        `TArrow` ( TApplication
                                                                    KType
                                                                    (TConstructor (KArrow KType KType) "Tree")
                                                                    (TVariable (TypeIndex KType 2) :| [])
                                                                 )
                                                        `TArrow` ( TApplication
                                                                    KType
                                                                    (TConstructor (KArrow KType KType) "Tree")
                                                                    (TVariable (TypeIndex KType 2) :| [])
                                                                 )
                                                        `TArrow` ( TApplication
                                                                    KType
                                                                    (TConstructor (KArrow KType KType) "Tree")
                                                                    (TVariable (TypeIndex KType 2) :| [])
                                                                 )
                                                    )
                                                    "Node"
                                                )
                                            )
                                            ( EVariable () (Label (TVariable (TypeIndex KType 2)) "p")
                                                <| EApplication
                                                  ()
                                                  ( TApplication
                                                      KType
                                                      (TConstructor (KArrow KType KType) "Tree")
                                                      (TVariable (TypeIndex KType 2) :| [])
                                                  )
                                                  ( EVariable
                                                      ()
                                                      ( Label
                                                          ( ( TIntrinsic
                                                                ( IRecord
                                                                    ( TRow
                                                                        ( RExtend
                                                                            "max"
                                                                            (TVariable (TypeIndex KType 2))
                                                                            (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                              `TArrow` ( TApplication
                                                                          KType
                                                                          (TConstructor (KArrow KType KType) "Tree")
                                                                          (TVariable (TypeIndex KType 2) :| [])
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
                                                                      (TVariable (TypeIndex KType 2))
                                                                      (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                                  )
                                                              )
                                                          )
                                                      )
                                                      ( Map.fromList
                                                          [
                                                            ( "min"
                                                            , ESelect
                                                                ()
                                                                (Label (TVariable (TypeIndex KType 2)) "min")
                                                                ( EVariable
                                                                    ()
                                                                    ( Label
                                                                        ( TIntrinsic
                                                                            ( IRecord
                                                                                ( TRow
                                                                                    ( RExtend
                                                                                        "max"
                                                                                        (TVariable (TypeIndex KType 2))
                                                                                        (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
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
                                                            , EVariable () (Label (TVariable (TypeIndex KType 2)) "p")
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
                                                      (TVariable (TypeIndex KType 2) :| [])
                                                  )
                                                  ( EVariable
                                                      ()
                                                      ( Label
                                                          ( ( TIntrinsic
                                                                ( IRecord
                                                                    ( TRow
                                                                        ( RExtend
                                                                            "max"
                                                                            (TVariable (TypeIndex KType 2))
                                                                            (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                              `TArrow` ( TApplication
                                                                          KType
                                                                          (TConstructor (KArrow KType KType) "Tree")
                                                                          (TVariable (TypeIndex KType 2) :| [])
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
                                                                      (TVariable (TypeIndex KType 2))
                                                                      (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                                  )
                                                              )
                                                          )
                                                      )
                                                      ( Map.fromList
                                                          [
                                                            ( "min"
                                                            , EVariable () (Label (TVariable (TypeIndex KType 2)) "p")
                                                            )
                                                          ,
                                                            ( "max"
                                                            , ESelect
                                                                ()
                                                                (Label (TVariable (TypeIndex KType 2)) "max")
                                                                ( EVariable
                                                                    ()
                                                                    ( Label
                                                                        ( TIntrinsic
                                                                            ( IRecord
                                                                                ( TRow
                                                                                    ( RExtend
                                                                                        "max"
                                                                                        (TVariable (TypeIndex KType 2))
                                                                                        (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
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
                                                (TVariable (TypeIndex KType 2) :| [])
                                            )
                                            ( EVariable
                                                ()
                                                ( Label
                                                    ( ( TIntrinsic
                                                          ( IRecord
                                                              ( TRow
                                                                  ( RExtend
                                                                      "max"
                                                                      (TVariable (TypeIndex KType 2))
                                                                      (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                                  )
                                                              )
                                                          )
                                                      )
                                                        `TArrow` ( TApplication
                                                                    KType
                                                                    (TConstructor (KArrow KType KType) "Tree")
                                                                    (TVariable (TypeIndex KType 2) :| [])
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
                                                                    (TVariable (TypeIndex KType 2))
                                                                    (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
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
                              (PListLiteral () (TIntrinsic (IList (TVariable (TypeIndex KType 2)))) [])
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
                                                        (TVariable (TypeIndex KType 2))
                                                        (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                    )
                                                )
                                            )
                                        )
                                          `TArrow` ( TApplication
                                                      KType
                                                      (TConstructor (KArrow KType KType) "Tree")
                                                      (TVariable (TypeIndex KType 2) :| [])
                                                   )
                                      )
                                      ( EVariable
                                          ()
                                          ( Label
                                              ( ( TApplication
                                                    KType
                                                    (TConstructor (KArrow KType KType) "Tree")
                                                    (TVariable (TypeIndex KType 2) :| [])
                                                )
                                                  `TArrow` TIntrinsic
                                                    ( IRecord
                                                        ( TRow
                                                            ( RExtend
                                                                "max"
                                                                (TVariable (TypeIndex KType 2))
                                                                (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                            )
                                                        )
                                                    )
                                                  `TArrow` ( TApplication
                                                              KType
                                                              (TConstructor (KArrow KType KType) "Tree")
                                                              (TVariable (TypeIndex KType 2) :| [])
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
                                                  (TVariable (TypeIndex KType 2) :| [])
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
                            ( ERecursiveLet
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
                                                                          `TArrow` (TVariable (TypeIndex KType 1))
                                                                          `TArrow` (TIntrinsic IBool)
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
                                ( EApplication
                                    ()
                                    ( TApplication
                                        KType
                                        (TConstructor (KArrow KType KType) "Tree")
                                        (TVariable (TypeIndex KType 2) :| [])
                                    )
                                    ( EVariable
                                        ()
                                        ( Label
                                            ( TIntrinsic (IList (TVariable (TypeIndex KType 2)))
                                                `TArrow` ( TIntrinsic
                                                            ( IRecord
                                                                ( TRow
                                                                    ( RExtend
                                                                        "max"
                                                                        (TVariable (TypeIndex KType 2))
                                                                        (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                                    )
                                                                )
                                                            )
                                                         )
                                                `TArrow` ( TApplication
                                                            KType
                                                            (TConstructor (KArrow KType KType) "Tree")
                                                            (TVariable (TypeIndex KType 2) :| [])
                                                         )
                                            )
                                            "$fold.1"
                                        )
                                    )
                                    ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 2)))) "list")
                                        <| ERecord
                                          ()
                                          ( TIntrinsic
                                              ( IRecord
                                                  ( TRow
                                                      ( RExtend
                                                          "max"
                                                          (TVariable (TypeIndex KType 2))
                                                          (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                      )
                                                  )
                                              )
                                          )
                                          ( Map.fromList
                                              [
                                                ( "max"
                                                , EApplication
                                                    ()
                                                    (TVariable (TypeIndex KType 2))
                                                    (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
                                                    (ELiteral () (LInt32 (-1)) :| [])
                                                )
                                              ,
                                                ( "min"
                                                , EApplication
                                                    ()
                                                    (TVariable (TypeIndex KType 2))
                                                    (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
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
                    ( ( TApplication
                          KType
                          (TConstructor (KArrow KType KType) "Tree")
                          (TVariable (TypeIndex KType 4) :| [])
                      )
                        `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 4)))
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
                                    (TVariable (TypeIndex KType 4) :| [])
                                )
                                "tree"
                            )
                        )
                        :| []
                    )
                    ( EFold
                        ()
                        (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
                        ( EVariable
                            ()
                            ( Label
                                ( TApplication
                                    KType
                                    (TConstructor (KArrow KType KType) "Tree")
                                    (TVariable (TypeIndex KType 4) :| [])
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
                                        (TVariable (TypeIndex KType 4) :| [])
                                    )
                                    "Node"
                                )
                                [ PVariable () (Label (TVariable (TypeIndex KType 4)) "y")
                                , PAtVariable
                                    ()
                                    ( Label
                                        ( TApplication
                                            KType
                                            (TConstructor (KArrow KType KType) "Tree")
                                            (TVariable (TypeIndex KType 4) :| [])
                                        )
                                        "lhs"
                                    )
                                , PAtVariable
                                    ()
                                    ( Label
                                        ( TApplication
                                            KType
                                            (TConstructor (KArrow KType KType) "Tree")
                                            (TVariable (TypeIndex KType 4) :| [])
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
                                    (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
                                    ( EBinaryOperator
                                        ()
                                        ( (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
                                            `TArrow` (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
                                            `TArrow` (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
                                        )
                                        OListConcatenation
                                    )
                                    ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 4)))) "lhs")
                                        <| EListCons
                                          ()
                                          (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
                                          (EVariable () (Label (TVariable (TypeIndex KType 4)) "y"))
                                          (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 4)))) "rhs"))
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
                                          (TVariable (TypeIndex KType 4) :| [])
                                      )
                                      "Leaf"
                                  )
                                  []
                              )
                              ( CPlain
                                  ()
                                  []
                                  (EListLiteral () (TIntrinsic (IList (TVariable (TypeIndex KType 4)))) [])
                                  :| []
                              )
                            :| []
                        )
                        ( Just
                            ( ERecursiveLet
                                ()
                                ( PVariable
                                    ()
                                    ( Label
                                        ( ( TApplication
                                              KType
                                              (TConstructor (KArrow KType KType) "Tree")
                                              (TVariable (TypeIndex KType 3) :| [])
                                          )
                                            `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
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
                                                (TVariable (TypeIndex KType 3) :| [])
                                            )
                                            "$fold.2.expr"
                                        )
                                        :| []
                                    )
                                    ( ECompiledMatch
                                        ()
                                        (TIntrinsic (IList (TVariable (TypeIndex KType 3))))
                                        ( EVariable
                                            ()
                                            ( Label
                                                ( TApplication
                                                    KType
                                                    (TConstructor (KArrow KType KType) "Tree")
                                                    (TVariable (TypeIndex KType 3) :| [])
                                                )
                                                "$fold.2.expr"
                                            )
                                        )
                                        ( ECompiledClause
                                            ( Label
                                                ( TApplication
                                                    KType
                                                    (TConstructor (KArrow KType KType) "Tree")
                                                    (TVariable (TypeIndex KType 3) :| [])
                                                )
                                                "Leaf"
                                                :| []
                                            )
                                            (EListLiteral () (TIntrinsic (IList (TVariable (TypeIndex KType 3)))) [])
                                            <| ECompiledClause
                                              ( Label
                                                  ( TVariable (TypeIndex KType 3)
                                                      `TArrow` TApplication
                                                        KType
                                                        (TConstructor (KArrow KType KType) "Tree")
                                                        (TVariable (TypeIndex KType 3) :| [])
                                                      `TArrow` TApplication
                                                        KType
                                                        (TConstructor (KArrow KType KType) "Tree")
                                                        (TVariable (TypeIndex KType 3) :| [])
                                                      `TArrow` TApplication
                                                        KType
                                                        (TConstructor (KArrow KType KType) "Tree")
                                                        (TVariable (TypeIndex KType 3) :| [])
                                                  )
                                                  "Node"
                                                  <| Label (TVariable (TypeIndex KType 3)) "$match.13.y"
                                                  <| Label
                                                    ( TApplication
                                                        KType
                                                        (TConstructor (KArrow KType KType) "Tree")
                                                        (TVariable (TypeIndex KType 3) :| [])
                                                    )
                                                    "$match.14.lhs"
                                                  <| Label
                                                    ( TApplication
                                                        KType
                                                        (TConstructor (KArrow KType KType) "Tree")
                                                        (TVariable (TypeIndex KType 3) :| [])
                                                    )
                                                    "$match.15.rhs"
                                                  :| []
                                              )
                                              ( EApplication
                                                  ()
                                                  (TIntrinsic (IList (TVariable (TypeIndex KType 3))))
                                                  ( EBinaryOperator
                                                      ()
                                                      ( TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                                                          `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                                                          `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                                                      )
                                                      OListConcatenation
                                                  )
                                                  ( EApplication
                                                      ()
                                                      (TIntrinsic (IList (TVariable (TypeIndex KType 3))))
                                                      ( EVariable
                                                          ()
                                                          ( Label
                                                              ( ( TApplication
                                                                    KType
                                                                    (TConstructor (KArrow KType KType) "Tree")
                                                                    (TVariable (TypeIndex KType 3) :| [])
                                                                )
                                                                  `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
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
                                                                  (TVariable (TypeIndex KType 3) :| [])
                                                              )
                                                              "$match.14.lhs"
                                                          )
                                                          :| []
                                                      )
                                                      <| EListCons
                                                        ()
                                                        (TIntrinsic (IList (TVariable (TypeIndex KType 3))))
                                                        (EVariable () (Label (TVariable (TypeIndex KType 3)) "$match.13.y"))
                                                        ( EApplication
                                                            ()
                                                            (TIntrinsic (IList (TVariable (TypeIndex KType 3))))
                                                            ( EVariable
                                                                ()
                                                                ( Label
                                                                    ( ( TApplication
                                                                          KType
                                                                          (TConstructor (KArrow KType KType) "Tree")
                                                                          (TVariable (TypeIndex KType 3) :| [])
                                                                      )
                                                                        `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
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
                                                                        (TVariable (TypeIndex KType 3) :| [])
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
                                ( EApplication
                                    ()
                                    (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
                                    ( EVariable
                                        ()
                                        ( Label
                                            ( ( TApplication
                                                  KType
                                                  (TConstructor (KArrow KType KType) "Tree")
                                                  (TVariable (TypeIndex KType 4) :| [])
                                              )
                                                `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 4)))
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
                                                (TVariable (TypeIndex KType 4) :| [])
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
                    []
                    ( TIntrinsic (IList (TVariable (TypeIndex KType 5)))
                        `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 5)))
                    )
                )
                ( EApplication
                    ()
                    ( TIntrinsic (IList (TVariable (TypeIndex KType 5)))
                        `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 5)))
                    )
                    ( EBinaryOperator
                        ()
                        ( ( ( TApplication
                                KType
                                (TConstructor (KArrow KType KType) "Tree")
                                (TVariable (TypeIndex KType 5) :| [])
                            )
                              `TArrow` (TIntrinsic (IList (TVariable (TypeIndex KType 5))))
                          )
                            `TArrow` ( (TIntrinsic (IList (TVariable (TypeIndex KType 5))))
                                        `TArrow` ( TApplication
                                                    KType
                                                    (TConstructor (KArrow KType KType) "Tree")
                                                    (TVariable (TypeIndex KType 5) :| [])
                                                 )
                                     )
                            `TArrow` ( (TIntrinsic (IList (TVariable (TypeIndex KType 5))))
                                        `TArrow` (TIntrinsic (IList (TVariable (TypeIndex KType 5))))
                                     )
                        )
                        OReverseComposition
                    )
                    ( EVariable
                        ()
                        ( Label
                            ( ( TApplication
                                  KType
                                  (TConstructor (KArrow KType KType) "Tree")
                                  (TVariable (TypeIndex KType 5) :| [])
                              )
                                `TArrow` (TIntrinsic (IList (TVariable (TypeIndex KType 5))))
                            )
                            "flatten"
                        )
                        <| EVariable
                          ()
                          ( Label
                              ( (TIntrinsic (IList (TVariable (TypeIndex KType 5))))
                                  `TArrow` ( TApplication
                                              KType
                                              (TConstructor (KArrow KType KType) "Tree")
                                              (TVariable (TypeIndex KType 5) :| [])
                                           )
                              )
                              "from_list"
                          )
                        :| []
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
      DImport (Path ["BinarySearch"]) ["sort"]
    , -- main
      DConstant
        "main"
        ( Constant
            ()
            (With [] (TIntrinsic IUnit `TArrow` TVariable (TypeIndex KType 0)))
            ( ELambda
                ()
                (PLiteral () LUnit :| [])
                ( ELet
                    ()
                    ( BPattern
                        ()
                        (PVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "xs"))
                        ( EAnnotation
                            ()
                            (TIntrinsic (IList (TVariable (Parameter () "a"))))
                            ( EListLiteral
                                ()
                                (TIntrinsic (IList (TVariable (TypeIndex KType 1))))
                                [ EApplication () (TVariable (TypeIndex KType 1)) (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1)) "from_int32")) (ELiteral () (LInt32 5) :| [])
                                , EApplication () (TVariable (TypeIndex KType 1)) (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1)) "from_int32")) (ELiteral () (LInt32 3) :| [])
                                , EApplication () (TVariable (TypeIndex KType 1)) (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1)) "from_int32")) (ELiteral () (LInt32 7) :| [])
                                , EApplication () (TVariable (TypeIndex KType 1)) (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1)) "from_int32")) (ELiteral () (LInt32 2) :| [])
                                , EApplication () (TVariable (TypeIndex KType 1)) (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1)) "from_int32")) (ELiteral () (LInt32 1) :| [])
                                , EApplication () (TVariable (TypeIndex KType 1)) (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1)) "from_int32")) (ELiteral () (LInt32 6) :| [])
                                , EApplication () (TVariable (TypeIndex KType 1)) (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1)) "from_int32")) (ELiteral () (LInt32 4) :| [])
                                ]
                            )
                        )
                        :| []
                    )
                    ( EApplication
                        ()
                        (TVariable (TypeIndex KType 0))
                        (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 2))) `TArrow` TVariable (TypeIndex KType 0)) "trace"))
                        ( EApplication
                            ()
                            (TIntrinsic (IList (TVariable (TypeIndex KType 2))))
                            (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 2))) `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 2)))) "sort"))
                            ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 2)))) "xs")
                                :| []
                            )
                            :| []
                        )
                    )
                )
            )
        )
    ]
