{-# LANGUAGE OverloadedStrings #-}

module Noll.Examples.Test09 (
  test09,
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
  CompiledClause (..),
  Constant (..),
  Definition (..),
  Expression (..),
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

import qualified Noll.Language.Module as Module

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
        ( Uses
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
                ( Uses
                    []
                    (tvar0 `TArrow` tvar0 `TArrow` TIntrinsic IBool)
                )
                ( EDictionaryLambda
                    ()
                    (Trait "Ordered" (TVariable (TypeIndex KType 0)) :| [])
                    ( ELambda
                        ()
                        ( PVariable () (Label tvar0 "m")
                            <| PVariable () (Label tvar0 "n")
                            :| []
                        )
                        ( ECompiledMatch
                            ()
                            (TIntrinsic IBool)
                            ( EDictionaryApplication
                                ()
                                (TConstructor KType "Ordering")
                                (EVariable () (Label (tvar0 `TArrow` tvar0 `TArrow` TConstructor KType "Ordering") "compare"))
                                (Trait "Ordered" (TVariable (TypeIndex KType 0)) :| [])
                                [EVariable () (Label tvar0 "m"), EVariable () (Label tvar0 "n")]
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
        ( DConstant
            "greater_than"
            ( Constant
                ()
                ( Uses
                    []
                    (tvar1 `TArrow` tvar1 `TArrow` TIntrinsic IBool)
                )
                ( EDictionaryLambda
                    ()
                    (Trait "Ordered" (TVariable (TypeIndex KType 0)) :| [])
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
                                , OReverseComposition
                                )
                            )
                            ( EVariable () (Label (TIntrinsic IBool `TArrow` TIntrinsic IBool) "not")
                                <| EDictionaryApplication
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
                                  (Trait "Ordered" (TVariable (TypeIndex KType 0)) :| [])
                                  [EVariable () (Label (TVariable (TypeIndex KType 1)) "n")]
                                :| []
                            )
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
        (Uses [] (TIntrinsic (IList (TVariable (Parameter () "a")))))
        ( DConstant
            "flatten_tree"
            ( Constant
                ()
                ( Uses
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
                                    ( ECompiledMatch
                                        ()
                                        list0
                                        (EVariable () (Label tree0 "$fold.1.expr"))
                                        ( ECompiledClause
                                            (Label tree0 "Leaf" :| [])
                                            (EListLiteral () list0 [])
                                            <| ECompiledClause
                                              ( Label tree0 "Node"
                                                  <| Label tvar0 "$match.2.y"
                                                  <| Label tree0 "$match.3.lhs"
                                                  <| Label tree0 "$match.4.rhs"
                                                  :| []
                                              )
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
                                                      (EVariable () (Label tree0 "$match.3.lhs") :| [])
                                                      <| EListCons
                                                        ()
                                                        list0
                                                        (EVariable () (Label tvar0 "$match.2.y"))
                                                        ( EApplication
                                                            ()
                                                            list0
                                                            (EVariable () (Label (tree0 `TArrow` list0) "$fold.1"))
                                                            (EVariable () (Label tree0 "$match.4.rhs") :| [])
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

-- Dictionary insertion
test09 :: [Module () Kind IndexedType]
test09 =
  [ moduleOrdered
  , moduleBinarySearch
  , moduleMain
  ]
