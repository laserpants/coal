{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler2.EnvironmentSpec where

import Lang.Common.Environment (Environment)
import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..), Path (..))
import Test.Hspec (Spec, describe, it)
import Noll.Compiler2.Environment

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Lang.Common.Environment as Environment

spec :: Spec
spec =
  describe "Noll.Compiler" $ do
    it "" $ do
       buildConstructorEnvs defs == (tenv1, tenv2)

defs :: [Definition () o ()]
defs =
  [ DImport (Path ["Core$"]) ["trace_int32", "trace_bool", "not", "always", "from_int32", "from_int32__$instance_Numeric(Intrinsic(Int32))"]
  , DType
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
  , DTrait
      "Ordered"
      []
      (TVariable (Parameter () "a"))
      [
        ( "compare"
        , TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TConstructor () "Ordering"
        )
      ]
  , DInstance
      "Ordered"
      (TIntrinsic IInt32)
      [ DFunction
          "compare"
          ( Function
              ()
              (With [] ())
              ( PVariable () (Label () "x")
                  <| PVariable () (Label () "y")
                  :| []
              )
              ( EIf
                  ()
                  ()
                  ( EApplication
                      ()
                      ()
                      (EBinaryOperator () () OLessThan)
                      ( EVariable () (Label () "x")
                          <| EVariable () (Label () "y")
                          :| []
                      )
                  )
                  (EConstructor () (Label () "LessThan"))
                  ( EIf
                      ()
                      ()
                      ( EApplication
                          ()
                          ()
                          (EBinaryOperator () () OGreaterThan)
                          ( EVariable () (Label () "x")
                              <| EVariable () (Label () "y")
                              :| []
                          )
                      )
                      (EConstructor () (Label () "GreaterThan"))
                      (EConstructor () (Label () "EqualTo"))
                  )
              )
          )
      ]
  , DType
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
  , DFunction
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
  , DFunction
      "greater_than"
      ( Function
          ()
          (With [] ())
          (PVariable () (Label () "n") :| [])
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
  , DFunction
      "in_range"
      ( Function
          ()
          (With [] ())
          ( PVariable () (Label () "range")
              <| PVariable () (Label () "n")
              :| []
          )
          ( EApplication
              ()
              ()
              (EBinaryOperator () () OLogicalAnd)
              ( EApplication
                  ()
                  ()
                  (EVariable () (Label () "greater_than"))
                  ( EVariable () (Label () "n")
                      <| ESelect () (Label () "min") (EVariable () (Label () "range"))
                      :| []
                  )
                  <| EApplication
                    ()
                    ()
                    (EBinaryOperator () () OLogicalOr)
                    ( EApplication
                        ()
                        ()
                        (EVariable () (Label () "less_than_or_equal_to"))
                        ( EVariable () (Label () "n")
                            <| ESelect () (Label () "max") (EVariable () (Label () "range"))
                            :| []
                        )
                        <| EApplication
                          ()
                          ()
                          (EVariable () (Label () "less_than_or_equal_to"))
                          ( ESelect () (Label () "max") (EVariable () (Label () "range"))
                              <| EApplication
                                ()
                                ()
                                (EVariable () (Label () "from_int32"))
                                ( ELiteral () (LInt32 (-1))
                                    :| []
                                )
                              :| []
                          )
                        :| []
                    )
                  :| []
              )
          )
      )
  , DFunction
      "from_list"
      ( Function
          ()
          (With [] ())
          (PVariable () (Label () "list") :| [])
          ( EFold
              ()
              ()
              ( EVariable () (Label () "list")
                  <| ERecord
                    ()
                    ()
                    ( Map.fromList
                        [
                          ( "min"
                          , EApplication
                              ()
                              ()
                              (EVariable () (Label () "from_int32"))
                              ( ELiteral () (LInt32 0)
                                  :| []
                              )
                          )
                        ,
                          ( "max"
                          , EApplication
                              ()
                              ()
                              (EVariable () (Label () "from_int32"))
                              ( ELiteral () (LInt32 (-1))
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
                      ()
                      (PVariable () (Label () "p"))
                      (PAtVariable () (Label () "g"))
                  )
                  ( CPlain
                      ()
                      []
                      ( ELambda
                          ()
                          (PVariable () (Label () "range") :| [])
                          ( EIf
                              ()
                              ()
                              ( EApplication
                                  ()
                                  ()
                                  (EBinaryOperator () () OReverseApplication)
                                  ( EVariable () (Label () "p")
                                      <| EApplication
                                        ()
                                        ()
                                        (EVariable () (Label () "in_range"))
                                        (EVariable () (Label () "range") :| [])
                                      :| []
                                  )
                              )
                              ( EApplication
                                  ()
                                  ()
                                  (EConstructor () (Label () "Node"))
                                  ( EVariable () (Label () "p")
                                      <| EApplication
                                        ()
                                        ()
                                        (EVariable () (Label () "g"))
                                        ( ERecord
                                            ()
                                            ()
                                            ( Map.fromList
                                                [
                                                  ( "min"
                                                  , ESelect () (Label () "min") (EVariable () (Label () "range"))
                                                  )
                                                ,
                                                  ( "max"
                                                  , EVariable () (Label () "p")
                                                  )
                                                ]
                                            )
                                            Nothing
                                            :| []
                                        )
                                      <| EApplication
                                        ()
                                        ()
                                        (EVariable () (Label () "g"))
                                        ( ERecord
                                            ()
                                            ()
                                            ( Map.fromList
                                                [
                                                  ( "min"
                                                  , EVariable () (Label () "p")
                                                  )
                                                ,
                                                  ( "max"
                                                  , ESelect () (Label () "max") (EVariable () (Label () "range"))
                                                  )
                                                ]
                                            )
                                            Nothing
                                            :| []
                                        )
                                      :| []
                                  )
                              )
                              (EApplication () () (EVariable () (Label () "g")) (EVariable () (Label () "range") :| []))
                          )
                      )
                      :| []
                  )
                  <| EClause
                    ()
                    (PListLiteral () () [])
                    ( CPlain
                        ()
                        []
                        ( EApplication
                            ()
                            ()
                            (EVariable () (Label () "always"))
                            (EConstructor () (Label () "Leaf") :| [])
                        )
                        :| []
                    )
                  :| []
              )
              Nothing
          )
      )
  , DFunction
      "flatten"
      ( Function
          ()
          (With [] ())
          (PVariable () (Label () "tree") :| [])
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
                              <| EListCons () () (EVariable () (Label () "y")) (EVariable () (Label () "rhs"))
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
  , DConstant
      "sort"
      ( Constant
          ()
          (With [] ())
          ( EApplication
              ()
              ()
              (EBinaryOperator () () OReverseComposition)
              ( EVariable () (Label () "flatten")
                  <| EVariable () (Label () "from_list")
                  :| []
              )
          )
      )
  , DFunction
      "main"
      ( Function
          ()
          (With [] ())
          (PLiteral () LUnit :| [])
          ( ELet
              ()
              ( BPattern
                  ()
                  (PVariable () (Label () "xs"))
                  ( EAnnotation
                      ()
                      (TIntrinsic (IList (TIntrinsic IInt32)))
                      ( EListLiteral
                          ()
                          ()
                          [ EApplication () () (EVariable () (Label () "from_int32")) (ELiteral () (LInt32 5) :| []) -- ELiteral () (LInt32 5)
                          , EApplication () () (EVariable () (Label () "from_int32")) (ELiteral () (LInt32 3) :| []) -- Literal () (LInt32 3)
                          , EApplication () () (EVariable () (Label () "from_int32")) (ELiteral () (LInt32 7) :| []) -- Literal () (LInt32 7)
                          , EApplication () () (EVariable () (Label () "from_int32")) (ELiteral () (LInt32 2) :| []) -- Literal () (LInt32 2)
                          , EApplication () () (EVariable () (Label () "from_int32")) (ELiteral () (LInt32 1) :| []) -- Literal () (LInt32 1)
                          , EApplication () () (EVariable () (Label () "from_int32")) (ELiteral () (LInt32 6) :| []) -- Literal () (LInt32 6)
                          , EApplication () () (EVariable () (Label () "from_int32")) (ELiteral () (LInt32 4) :| []) -- Literal () (LInt32 4)
                          ]
                      )
                  )
                  :| []
              )
              ( EApplication
                  ()
                  ()
                  (EVariable () (Label () "trace_int32"))
                  ( EMatch
                      ()
                      ()
                      ( EApplication
                          ()
                          ()
                          (EVariable () (Label () "sort"))
                          (EVariable () (Label () "xs") :| [])
                      )
                      ( EClause
                          ()
                          (PListLiteral () () [])
                          ( CPlain
                              ()
                              []
                              (ELiteral () (LInt32 12445))
                              :| []
                          )
                          <| EClause
                            ()
                            (PListCons () () (PVariable () (Label () "y")) (PAny () ()))
                            ( CPlain
                                ()
                                []
                                (EVariable () (Label () "y"))
                                :| []
                            )
                          :| []
                      )
                      :| []
                  )
              )
          )
      )
  ]

tenv1 =
  Environment.fromList
    [
      ( "Tree"
      , KArrow KType KType
      )
    ,
      ( "Ordering"
      , KType
      )
    ]

tree0 :: IndexedType
tree0 =
  TApplication
    KType
    (TConstructor (KArrow KType KType) "Tree")
    (TVariable (TypeIndex KType 0) :| [])

tenv2 :: Environment (Constructor TypeIndex Kind IndexedType)
tenv2 =
  Environment.fromList
    [
      ( "LessThan"
      , Constructor
          "LessThan"
          0
          (Forall mempty [] (TConstructor KType "Ordering"))
      )
    ,
      ( "GreaterThan"
      , Constructor
          "GreaterThan"
          0
          (Forall mempty [] (TConstructor KType "Ordering"))
      )
    ,
      ( "EqualTo"
      , Constructor
          "EqualTo"
          0
          (Forall mempty [] (TConstructor KType "Ordering"))
      )
    ,
      ( "Node"
      , Constructor
          "Node"
          3
          ( Forall
              (Set.fromList [TypeIndex KType 0])
              []
              ( TVariable (TypeIndex KType 0)
                  `TArrow` tree0
                  `TArrow` tree0
                  `TArrow` tree0
              )
          )
      )
    ,
      ( "Leaf"
      , Constructor
          "Leaf"
          0
          ( Forall
              (Set.fromList [TypeIndex KType 0])
              []
              tree0
          )
      )
    ]
