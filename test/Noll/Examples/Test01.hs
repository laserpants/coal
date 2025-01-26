{-# LANGUAGE OverloadedStrings #-}

module Noll.Examples.Test01 (
  test01,
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
    -- Exports
    []
    -- Definitions
    [ -- type Ordering
      -- trait Ordered
      -- instance Ordered(int32)
      -- less_than_or_equal_to
      -- greater_than
      ( DAnnotation
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
    -- Exports
    ["Tree", "build_tree", "flatten_tree"]
    -- Definitions
    [ -- type Tree
      -- type_alias Range
      -- invalid_range
      -- is_invalid
      -- in_range
      -- build_tree
      -- flatten_tree
      ( DAnnotation
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
                      Nothing
                  )
              )
          )
      )
      -- sort
    ]

moduleMain :: Module () () ()
moduleMain =
  Module.fromDefinitionList
    (Path ["Main"])
    -- Exports
    []
    -- Definitions
    []

-- Untyped source tree
test01 :: [Module () () ()]
test01 =
  [ moduleOrdered
  , moduleBinarySearch
  , moduleMain
  ]
