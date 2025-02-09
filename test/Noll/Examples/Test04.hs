{-# LANGUAGE OverloadedStrings #-}

module Noll.Examples.Test04 (
  test04,
  moduleOrdered,
  moduleBinarySearch,
  moduleMain,
) where

import Noll.Common.List1 (NonEmpty (..), (<|))
import Noll.Label (Label (..))
import Noll.Language (
  BinaryOperator (..),
  Choice (..),
  Clause (..),
  Definition (..),
  Expression (..),
  Function (..),
  IndexedType,
  Intrinsic (..),
  Kind (..),
  Module (..),
  Parameter (..),
  Path (..),
  Pattern (..),
  Primitive (..),
  Trait (..),
  Type (..),
  TypeIndex (..),
  Uses (..),
 )

import qualified Noll.Module as Module

moduleOrdered :: Module () Kind IndexedType
moduleOrdered =
  Module.fromDefinitionList
    (Path ["Ordered"])
    []
    [ DAnnotation
        ( Uses
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
                ( Uses
                    []
                    (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
                )
                (PVariable () (Label (TVariable (TypeIndex KType 0)) "m") :| [])
                ( ELambda
                    ()
                    (PVariable () (Label (TVariable (TypeIndex KType 0)) "n") :| [])
                    ( EMatch
                        ()
                        (TIntrinsic IBool)
                        ( EApplication
                            ()
                            (TConstructor KType "Ordering")
                            ( EVariable
                                ()
                                ( Label
                                    ( TVariable (TypeIndex KType 0)
                                        `TArrow` TVariable (TypeIndex KType 0)
                                        `TArrow` TConstructor KType "Ordering"
                                    )
                                    "compare"
                                )
                            )
                            ( EVariable () (Label (TVariable (TypeIndex KType 0)) "m")
                                <| EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                                :| []
                            )
                        )
                        ( EClause
                            ()
                            ( POr
                                ()
                                (TConstructor KType "Ordering")
                                (PConstructor () (Label (TConstructor KType "Ordering") "LessThan") [])
                                (PConstructor () (Label (TConstructor KType "Ordering") "EqualTo") [])
                            )
                            (CPlain () [] (ELiteral () (LBool True)) :| [])
                            <| EClause
                              ()
                              (PConstructor () (Label (TConstructor KType "Ordering") "GreaterThan") [])
                              (CPlain () [] (ELiteral () (LBool False)) :| [])
                            :| []
                        )
                    )
                )
            )
        )
    , DAnnotation
        ( Uses
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
                ( Uses
                    []
                    (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool)
                )
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
                        , OReverseComposition
                        )
                    )
                    ( EVariable () (Label (TIntrinsic IBool `TArrow` TIntrinsic IBool) "not")
                        <| EApplication
                          ()
                          (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool)
                          ( EVariable
                              ()
                              ( Label
                                  ( TVariable (TypeIndex KType 1)
                                      `TArrow` TVariable (TypeIndex KType 1)
                                      `TArrow` TIntrinsic IBool
                                  )
                                  "less_than_or_equal_to"
                              )
                          )
                          (EVariable () (Label (TVariable (TypeIndex KType 1)) "n") :| [])
                        :| []
                    )
                )
            )
        )
    ]

tree0 :: Type TypeIndex Kind
tree0 = TApplication KType (TConstructor (KArrow KType KType) "Tree") (TVariable (TypeIndex KType 0) :| [])

list0 :: Type TypeIndex Kind
list0 = TIntrinsic (IList (TVariable (TypeIndex KType 0)))

tvar0 :: Type TypeIndex Kind
tvar0 = TVariable (TypeIndex KType 0)

tree1 :: Type TypeIndex Kind
tree1 = TApplication KType (TConstructor (KArrow KType KType) "Tree") (TVariable (TypeIndex KType 1) :| [])

list1 :: Type TypeIndex Kind
list1 = TIntrinsic (IList (TVariable (TypeIndex KType 1)))

tvar1 :: Type TypeIndex Kind
tvar1 = TVariable (TypeIndex KType 1)

moduleBinarySearch :: Module () Kind IndexedType
moduleBinarySearch =
  Module.fromDefinitionList
    (Path ["BinarySearch"])
    ["Tree", "build_tree", "flatten_tree"]
    [ DAnnotation
        (Uses [] (TIntrinsic (IList (TVariable (Parameter () "a")))))
        ( DFunction
            "flatten_tree"
            ( Function
                ()
                (Uses [] list1)
                ( PAnnotation
                    ()
                    ( TApplication
                        ()
                        (TConstructor () "Tree")
                        (TVariable (Parameter () "a") :| [])
                    )
                    ( PVariable
                        ()
                        (Label tree1 "tree")
                    )
                    :| []
                )
                ( EFold
                    ()
                    list1
                    ( EVariable
                        ()
                        (Label tree1 "tree")
                        :| []
                    )
                    ( EClause
                        ()
                        ( PConstructor
                            ()
                            ( Label
                                tree1
                                "Node"
                            )
                            [ PVariable () (Label tvar1 "y")
                            , PAtVariable
                                ()
                                (Label tree1 "lhs")
                            , PAtVariable
                                ()
                                (Label tree1 "rhs")
                            ]
                        )
                        ( CPlain
                            ()
                            []
                            ( EApplication
                                ()
                                list1
                                ( EBinaryOperator
                                    ()
                                    ( list1 `TArrow` list1 `TArrow` list1
                                    , OListConcatenation
                                    )
                                )
                                ( EVariable () (Label list1 "lhs")
                                    <| EListCons
                                      ()
                                      list1
                                      (EVariable () (Label tvar1 "y"))
                                      (EVariable () (Label list1 "rhs"))
                                    :| []
                                )
                            )
                            :| []
                        )
                        <| EClause
                          ()
                          ( PConstructor
                              ()
                              (Label tree1 "Leaf")
                              []
                          )
                          ( CPlain
                              ()
                              []
                              (EListLiteral () list1 [])
                              :| []
                          )
                        :| []
                    )
                    ( Just
                        ( ERecursiveLet
                            ()
                            (PVariable () (Label (tree0 `TArrow` list0) "$fold.1"))
                            ( ELambda
                                ()
                                ( PVariable
                                    ()
                                    (Label tree0 "$fold.1.expr")
                                    :| []
                                )
                                ( EMatch
                                    ()
                                    list0
                                    (EVariable () (Label tree0 "$fold.1.expr"))
                                    ( EClause
                                        ()
                                        ( PConstructor
                                            ()
                                            (Label tree0 "Node")
                                            [ PVariable () (Label tvar0 "y")
                                            , PVariable () (Label tree0 "lhs")
                                            , PVariable () (Label tree0 "rhs")
                                            ]
                                        )
                                        ( CPlain
                                            ()
                                            []
                                            ( EApplication
                                                ()
                                                list0
                                                ( EBinaryOperator
                                                    ()
                                                    ( list0 `TArrow` list0 `TArrow` list0
                                                    , OListConcatenation
                                                    )
                                                )
                                                ( EApplication
                                                    ()
                                                    list0
                                                    (EVariable () (Label (tree0 `TArrow` list0) "$fold.1"))
                                                    (EVariable () (Label tree0 "lhs") :| [])
                                                    <| EListCons
                                                      ()
                                                      list0
                                                      (EVariable () (Label tvar0 "y"))
                                                      ( EApplication
                                                          ()
                                                          list0
                                                          (EVariable () (Label (tree0 `TArrow` list0) "$fold.1"))
                                                          (EVariable () (Label tree0 "rhs") :| [])
                                                      )
                                                    :| []
                                                )
                                            )
                                            :| []
                                        )
                                        <| EClause
                                          ()
                                          (PConstructor () (Label tree0 "Leaf") [])
                                          (CPlain () [] (EListLiteral () list0 []) :| [])
                                        :| []
                                    )
                                )
                            )
                            ( EApplication
                                ()
                                list1
                                ( EVariable
                                    ()
                                    ( Label
                                        (tree1 `TArrow` list1)
                                        "$fold.1"
                                    )
                                )
                                ( EVariable
                                    ()
                                    (Label tree1 "tree")
                                    :| []
                                )
                            )
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
    []
    []

-- Add type info
test04 :: [Module () Kind IndexedType]
test04 =
  [ moduleOrdered
  , moduleBinarySearch
  , moduleMain
  ]
