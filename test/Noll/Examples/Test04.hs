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

import qualified Noll.Language.Module as Module

moduleOrdered :: Module () () IndexedType
moduleOrdered =
  Module.fromDefinitionList
    (Path ["Ordered"])
    []
    [ ( DAnnotation
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
                      ( TAlias
                          "Predicate"
                          [TVariable (TypeIndex KType 0)]
                          (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
                      )
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
                                  undefined
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
      )
    , ( DAnnotation
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
                      ( TAlias
                          "Predicate"
                          [TVariable (TypeIndex KType 0)]
                          (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
                      )
                  )
                  ( PAnnotation
                      ()
                      (TVariable (Parameter () "a"))
                      (PVariable () (Label (TVariable (TypeIndex KType 0)) "n"))
                      :| []
                  )
                  ( EApplication
                      ()
                      (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
                      ( EBinaryOperator
                          ()
                          ( (TIntrinsic IBool `TArrow` TIntrinsic IBool)
                              `TArrow` (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
                              `TArrow` TVariable (TypeIndex KType 0)
                              `TArrow` TIntrinsic IBool
                          , OReverseComposition
                          )
                      )
                      ( EVariable () (Label (TIntrinsic IBool `TArrow` TIntrinsic IBool) "not")
                          <| EApplication
                            ()
                            (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
                            ( EVariable
                                ()
                                ( Label
                                    ( TVariable (TypeIndex KType 0)
                                        `TArrow` TVariable (TypeIndex KType 0)
                                        `TArrow` TIntrinsic IBool
                                    )
                                    "less_than_or_equal_to"
                                )
                            )
                            (EVariable () (Label (TVariable (TypeIndex KType 0)) "n") :| [])
                          :| []
                      )
                  )
              )
          )
      )
    ]

moduleBinarySearch :: Module () () IndexedType
moduleBinarySearch =
  Module.fromDefinitionList
    (Path ["BinarySearch"])
    ["Tree", "build_tree", "flatten_tree"]
    [ ( DAnnotation
          (Uses [] (TIntrinsic (IList (TVariable (Parameter () "a")))))
          ( DFunction
              "flatten_tree"
              ( Function
                  ()
                  (Uses [] undefined)
                  ( PAnnotation
                      ()
                      ( TApplication
                          ()
                          (TConstructor () "Tree")
                          (TVariable (Parameter () "a") :| [])
                      )
                      (PVariable () (Label undefined "tree"))
                      :| []
                  )
                  ( EFold
                      ()
                      undefined
                      (EVariable () (Label undefined "tree") :| [])
                      ( EClause
                          ()
                          ( PConstructor
                              ()
                              (Label undefined "Node")
                              [ PVariable () (Label undefined "y")
                              , PAtVariable () (Label undefined "lhs")
                              , PAtVariable () (Label undefined "rhs")
                              ]
                          )
                          ( CPlain
                              ()
                              []
                              ( EApplication
                                  ()
                                  undefined
                                  (EBinaryOperator () (undefined, OListConcatenation))
                                  ( EVariable () (Label undefined "lhs")
                                      <| EListCons
                                        ()
                                        undefined
                                        (EVariable () (Label undefined "y"))
                                        (EVariable () (Label undefined "rhs"))
                                      :| []
                                  )
                              )
                              :| []
                          )
                          <| EClause
                            ()
                            (PConstructor () (Label undefined "Leaf") [])
                            ( CPlain
                                ()
                                []
                                (EListLiteral () undefined [])
                                :| []
                            )
                          :| []
                      )
                      ( Just
                          ( ERecursiveLet
                              ()
                              (PVariable () (Label undefined "$fold.1"))
                              ( ELambda
                                  ()
                                  (PVariable () (Label undefined "$fold.1.expr") :| [])
                                  ( EMatch
                                      ()
                                      undefined
                                      (EVariable () (Label undefined "$fold.1.expr"))
                                      ( EClause
                                          ()
                                          ( PConstructor
                                              ()
                                              (Label undefined "Node")
                                              [ PVariable () (Label undefined "y")
                                              , PVariable () (Label undefined "lhs")
                                              , PVariable () (Label undefined "rhs")
                                              ]
                                          )
                                          ( CPlain
                                              ()
                                              []
                                              ( EApplication
                                                  ()
                                                  undefined
                                                  ( EBinaryOperator
                                                      ()
                                                      ( undefined
                                                      , OListConcatenation
                                                      )
                                                  )
                                                  ( EApplication
                                                      ()
                                                      undefined
                                                      (EVariable () (Label undefined "$fold.1"))
                                                      (EVariable () (Label undefined "lhs") :| [])
                                                      <| EListCons
                                                        ()
                                                        undefined
                                                        (EVariable () (Label undefined "y"))
                                                        ( EApplication
                                                            ()
                                                            undefined
                                                            (EVariable () (Label undefined "$fold.1"))
                                                            (EVariable () (Label undefined "rhs") :| [])
                                                        )
                                                      :| []
                                                  )
                                              )
                                              :| []
                                          )
                                          <| EClause
                                            ()
                                            (PConstructor () (Label undefined "Leaf") [])
                                            (CPlain () [] (EListLiteral () undefined []) :| [])
                                          :| []
                                      )
                                  )
                              )
                              ( EApplication
                                  ()
                                  undefined
                                  (EVariable () (Label undefined "$fold.1"))
                                  (EVariable () (Label undefined "tree") :| [])
                              )
                          )
                      )
                  )
              )
          )
      )
    ]

moduleMain :: Module () () IndexedType
moduleMain =
  Module.fromDefinitionList
    (Path ["Main"])
    []
    []

-- Add type info
test04 :: [Module () () IndexedType]
test04 =
  [ moduleOrdered
  , moduleBinarySearch
  , moduleMain
  ]
