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
  Expression (..),
  Function (..),
  IndexedType,
  Intrinsic (..),
  Module (..),
  Object (..),
  Parameter (..),
  Path (..),
  Pattern (..),
  Trait (..),
  Type (..),
  Uses (..),
 )

import qualified Noll.Language.Module as Module

moduleOrdered :: Module () () IndexedType
moduleOrdered =
  Module.fromObjectList
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
              "greater_than"
              ( Function
                  ()
                  (Uses [] undefined)
                  ( PAnnotation
                      ()
                      (TVariable (Parameter () "a"))
                      (PVariable () (Label undefined "n"))
                      :| []
                  )
                  ( EApplication
                      ()
                      undefined
                      (EBinaryOperator () (undefined, OReverseComposition))
                      ( EVariable () (Label undefined "not")
                          <| EApplication
                            ()
                            undefined
                            (EVariable () (Label undefined "less_than_or_equal_to"))
                            (EVariable () (Label undefined "n") :| [])
                          :| []
                      )
                  )
              )
          )
      )
    ]

moduleBinarySearch :: Module () () IndexedType
moduleBinarySearch =
  Module.fromObjectList
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
  Module.fromObjectList
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
