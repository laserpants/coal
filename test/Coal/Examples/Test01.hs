{-# LANGUAGE OverloadedStrings #-}

module Coal.Examples.Test01 (
  test01,
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
  Intrinsic (..),
  Parameter (..),
  Pattern (..),
  Primitive (..),
  Trait (..),
  Type (..),
  With (..),
 )
import Coal.Language.Module (Definition (..), Function (..), Module (..), Path (..))

import qualified Coal.Language.Module as Module

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
      DAnnotation
        ( With
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
                (With [] ())
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
        ( With
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
                (With [] ())
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
    ["Tree", "build_tree", "flatten_tree"]
    -- Definitions
    [ -- type Tree
      -- type_alias Range
      -- invalid_range
      -- is_invalid
      -- in_range
      -- build_tree
      -- flatten_tree
      DAnnotation
        (With [] (TIntrinsic (IList (TVariable (Parameter () "a")))))
        ( DFunction
            "flatten_tree"
            ( Function
                ()
                (With [] ())
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
                                (EBinaryOperator () () OListConcatenation)
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
