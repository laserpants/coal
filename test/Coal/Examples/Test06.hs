{-# LANGUAGE OverloadedStrings #-}

module Coal.Examples.Test06 (
  test06,
  moduleOrdered,
  moduleBinarySearch,
  moduleMain,
) where

import Coal.Common.List1 (NonEmpty (..), (<|))
import Coal.Common.Label (Label (..))
import Coal.Language (
  BinaryOperator (..),
  Choice (..),
  Clause (..),
  Expression (..),
  IndexedType,
  Intrinsic (..),
  Kind (..),
  Parameter (..),
  Pattern (..),
  Primitive (..),
  Trait (..),
  Type (..),
  TypeIndex (..),
  With (..),
 )
import Coal.Language.Module (Constant (..), Definition (..), Module (..), Path (..))

import qualified Coal.Language.Module as Module

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

moduleOrdered :: Module () Kind IndexedType
moduleOrdered =
  Module.fromDefinitionList
    (Path ["Ordered"])
    []
    [ DAnnotation
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
                    []
                    (tvar0 `TArrow` tvar0 `TArrow` TIntrinsic IBool)
                )
                ( ELambda
                    ()
                    ( PVariable () (Label tvar0 "m")
                        <| PVariable () (Label tvar0 "n")
                        :| []
                    )
                    ( EMatch
                        ()
                        (TIntrinsic IBool)
                        ( EApplication
                            ()
                            (TConstructor KType "Ordering")
                            ( EVariable
                                ()
                                ( Label
                                    (tvar0 `TArrow` tvar0 `TArrow` TConstructor KType "Ordering")
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
                    []
                    (tvar1 `TArrow` tvar1 `TArrow` TIntrinsic IBool)
                )
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
        )
    ]

moduleBinarySearch :: Module () Kind IndexedType
moduleBinarySearch =
  Module.fromDefinitionList
    (Path ["BinarySearch"])
    ["Tree", "build_tree", "flatten_tree"]
    [ DAnnotation
        (With [] (TIntrinsic (IList (TVariable (Parameter () "a")))))
        ( DConstant
            "flatten_tree"
            ( Constant
                ()
                ( With
                    []
                    (tree1 `TArrow` list1)
                )
                ( ELambda
                    ()
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
                                        )
                                        OListConcatenation
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
                                                        )
                                                        OListConcatenation
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
        )
    ]

moduleMain :: Module () Kind IndexedType
moduleMain =
  Module.fromDefinitionList
    (Path ["Main"])
    []
    []

-- Translate patterns to match statements
test06 :: [Module () Kind IndexedType]
test06 =
  [ moduleOrdered
  , moduleBinarySearch
  , moduleMain
  ]
