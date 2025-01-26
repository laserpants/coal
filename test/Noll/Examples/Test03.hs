{-# LANGUAGE OverloadedStrings #-}

module Noll.Examples.Test03 (
  test03,
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
  Intrinsic (..),
  Module (..),
  Parameter (..),
  Path (..),
  Pattern (..),
  Trait (..),
  Type (..),
  Uses (..),
 )

import qualified Noll.Language.Module as Module

moduleOrdered :: Module () () ()
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
                      (EBinaryOperator () ((), OReverseComposition))
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
      )
    ]

moduleBinarySearch :: Module () () ()
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
                  (Uses [] ())
                  ( PAnnotation
                      ()
                      ( TApplication
                          ()
                          (TConstructor () "Tree")
                          (TVariable (Parameter () "a") :| [])
                      )
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
                                  (EBinaryOperator () ((), OListConcatenation))
                                  ( EVariable () (Label () "lhs")
                                      <| EListCons
                                        ()
                                        ()
                                        (EVariable () (Label () "y"))
                                        (EVariable () (Label () "rhs"))
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
                      ( Just
                          ( ERecursiveLet
                              ()
                              (PVariable () (Label () "$fold.1"))
                              ( ELambda
                                  ()
                                  (PVariable () (Label () "$fold.1.expr") :| [])
                                  ( EMatch
                                      ()
                                      ()
                                      (EVariable () (Label () "$fold.1.expr"))
                                      ( EClause
                                          ()
                                          ( PConstructor
                                              ()
                                              (Label () "Node")
                                              [ PVariable () (Label () "y")
                                              , PVariable () (Label () "lhs")
                                              , PVariable () (Label () "rhs")
                                              ]
                                          )
                                          ( CPlain
                                              ()
                                              []
                                              ( EApplication
                                                  ()
                                                  ()
                                                  ( EBinaryOperator
                                                      ()
                                                      ( ()
                                                      , OListConcatenation
                                                      )
                                                  )
                                                  ( EApplication
                                                      ()
                                                      ()
                                                      (EVariable () (Label () "$fold.1"))
                                                      (EVariable () (Label () "lhs") :| [])
                                                      <| EListCons
                                                        ()
                                                        ()
                                                        (EVariable () (Label () "y"))
                                                        ( EApplication
                                                            ()
                                                            ()
                                                            (EVariable () (Label () "$fold.1"))
                                                            (EVariable () (Label () "rhs") :| [])
                                                        )
                                                      :| []
                                                  )
                                              )
                                              :| []
                                          )
                                          <| EClause
                                            ()
                                            (PConstructor () (Label () "Leaf") [])
                                            (CPlain () [] (EListLiteral () () []) :| [])
                                          :| []
                                      )
                                  )
                              )
                              ( EApplication
                                  ()
                                  ()
                                  (EVariable () (Label () "$fold.1"))
                                  (EVariable () (Label () "tree") :| [])
                              )
                          )
                      )
                  )
              )
          )
      )
    ]

moduleMain :: Module () () ()
moduleMain =
  Module.fromDefinitionList
    (Path ["Main"])
    []
    []

-- Expand folds
test03 :: [Module () () ()]
test03 =
  [ moduleOrdered
  , moduleBinarySearch
  , moduleMain
  ]
