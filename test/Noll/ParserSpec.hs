{-# LANGUAGE OverloadedStrings #-}

module Noll.ParserSpec where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Module
import Noll.Parser
import Noll.Parser.Expression
import Noll.Parser.Module
import Test.Hspec (Spec, describe, it)
import Text.Megaparsec (runParser)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Noll.Module as Module

spec :: Spec
spec =
  describe "Noll.Compiler" $ do
    it "" $ do
      runParser parseExpression "" "fold(pack_nat(n)) { | Zero => 1 | Succ(@f) as m => unpack_nat(m) * f }"
        == Right
          ( EFold
              ()
              ()
              ( EApplication
                  ()
                  ()
                  (EVariable () (Label () "pack_nat"))
                  ( EVariable () (Label () "n")
                      :| []
                  )
                  :| []
              )
              ( EClause
                  ()
                  ( PConstructor
                      ()
                      (Label () "Zero")
                      []
                  )
                  ( CPlain
                      ()
                      []
                      ( EApplication
                          ()
                          ()
                          (EVariable () (Label () "from_int32"))
                          (ELiteral () (LInt32 1) :| [])
                      )
                      :| []
                  )
                  <| EClause
                    ()
                    ( PAs
                        ()
                        (Label () "m")
                        ( PConstructor
                            ()
                            (Label () "Succ")
                            [ PAtVariable () (Label () "f")
                            ]
                        )
                    )
                    ( CPlain
                        ()
                        []
                        ( EApplication
                            ()
                            ()
                            (EBinaryOperator () () OMultiplication)
                            ( EApplication
                                ()
                                ()
                                (EVariable () (Label () "unpack_nat"))
                                ( EVariable () (Label () "m")
                                    :| []
                                )
                                <| EVariable () (Label () "f")
                                :| []
                            )
                        )
                        :| []
                    )
                  :| []
              )
              Nothing
          )
    it "" $ do
      runParser parseFunctionDefinition "" "fn factorial(n) = fold(pack_nat(n)) { | Zero => 1 | Succ(@f) as m => unpack_nat(m) * f };"
        == Right
          ( DFunction
              "factorial"
              ( Function
                  ()
                  (With [] ())
                  (PVariable () (Label () "n") :| [])
                  ( EFold
                      ()
                      ()
                      ( EApplication
                          ()
                          ()
                          (EVariable () (Label () "pack_nat"))
                          ( EVariable () (Label () "n")
                              :| []
                          )
                          :| []
                      )
                      ( EClause
                          ()
                          ( PConstructor
                              ()
                              (Label () "Zero")
                              []
                          )
                          ( CPlain
                              ()
                              []
                              ( EApplication
                                  ()
                                  ()
                                  (EVariable () (Label () "from_int32"))
                                  (ELiteral () (LInt32 1) :| [])
                              )
                              :| []
                          )
                          <| EClause
                            ()
                            ( PAs
                                ()
                                (Label () "m")
                                ( PConstructor
                                    ()
                                    (Label () "Succ")
                                    [ PAtVariable () (Label () "f")
                                    ]
                                )
                            )
                            ( CPlain
                                ()
                                []
                                ( EApplication
                                    ()
                                    ()
                                    (EBinaryOperator () () OMultiplication)
                                    ( EApplication
                                        ()
                                        ()
                                        (EVariable () (Label () "unpack_nat"))
                                        ( EVariable () (Label () "m")
                                            :| []
                                        )
                                        <| EVariable () (Label () "f")
                                        :| []
                                    )
                                )
                                :| []
                            )
                          :| []
                      )
                      Nothing
                  )
              ) ::
              Definition () Kind ()
          )
    it "" $ do
      runParser parseFunctionDefinition "" "fn factorial(n : int32) = fold(pack_nat(n)) { | Zero => 1 | Succ(@f) as m => unpack_nat(m) * f };"
        == Right
          ( DFunction
              "factorial"
              ( Function
                  ()
                  (With [] ())
                  ( PAnnotation
                      ()
                      (TIntrinsic IInt32)
                      (PVariable () (Label () "n"))
                      :| []
                  )
                  ( EFold
                      ()
                      ()
                      ( EApplication
                          ()
                          ()
                          (EVariable () (Label () "pack_nat"))
                          ( EVariable () (Label () "n")
                              :| []
                          )
                          :| []
                      )
                      ( EClause
                          ()
                          ( PConstructor
                              ()
                              (Label () "Zero")
                              []
                          )
                          ( CPlain
                              ()
                              []
                              ( EApplication
                                  ()
                                  ()
                                  (EVariable () (Label () "from_int32"))
                                  (ELiteral () (LInt32 1) :| [])
                              )
                              :| []
                          )
                          <| EClause
                            ()
                            ( PAs
                                ()
                                (Label () "m")
                                ( PConstructor
                                    ()
                                    (Label () "Succ")
                                    [ PAtVariable () (Label () "f")
                                    ]
                                )
                            )
                            ( CPlain
                                ()
                                []
                                ( EApplication
                                    ()
                                    ()
                                    (EBinaryOperator () () OMultiplication)
                                    ( EApplication
                                        ()
                                        ()
                                        (EVariable () (Label () "unpack_nat"))
                                        ( EVariable () (Label () "m")
                                            :| []
                                        )
                                        <| EVariable () (Label () "f")
                                        :| []
                                    )
                                )
                                :| []
                            )
                          :| []
                      )
                      Nothing
                  )
              ) ::
              Definition () Kind ()
          )
    it "" $ do
      runParser parseFunctionDefinition "" "fn factorial(n : int32) : int32 = fold(pack_nat(n)) { | Zero => 1 | Succ(@f) as m => unpack_nat(m) * f };"
        == Right
          ( DAnnotation
              (With [] (TIntrinsic IInt32))
              ( DFunction
                  "factorial"
                  ( Function
                      ()
                      (With [] ())
                      ( PAnnotation
                          ()
                          (TIntrinsic IInt32)
                          (PVariable () (Label () "n"))
                          :| []
                      )
                      ( EFold
                          ()
                          ()
                          ( EApplication
                              ()
                              ()
                              (EVariable () (Label () "pack_nat"))
                              ( EVariable () (Label () "n")
                                  :| []
                              )
                              :| []
                          )
                          ( EClause
                              ()
                              ( PConstructor
                                  ()
                                  (Label () "Zero")
                                  []
                              )
                              ( CPlain
                                  ()
                                  []
                                  ( EApplication
                                      ()
                                      ()
                                      (EVariable () (Label () "from_int32"))
                                      (ELiteral () (LInt32 1) :| [])
                                  )
                                  :| []
                              )
                              <| EClause
                                ()
                                ( PAs
                                    ()
                                    (Label () "m")
                                    ( PConstructor
                                        ()
                                        (Label () "Succ")
                                        [ PAtVariable () (Label () "f")
                                        ]
                                    )
                                )
                                ( CPlain
                                    ()
                                    []
                                    ( EApplication
                                        ()
                                        ()
                                        (EBinaryOperator () () OMultiplication)
                                        ( EApplication
                                            ()
                                            ()
                                            (EVariable () (Label () "unpack_nat"))
                                            ( EVariable () (Label () "m")
                                                :| []
                                            )
                                            <| EVariable () (Label () "f")
                                            :| []
                                        )
                                    )
                                    :| []
                                )
                              :| []
                          )
                          Nothing
                      )
                  )
              ) ::
              Definition () Kind ()
          )
    it "" $ do
      runParser parseFunctionDefinition "" "fn main() = trace_int32(factorial(12));"
        == Right
          ( DFunction
              "main"
              ( Function
                  ()
                  (With [] ())
                  (PLiteral () LUnit :| [])
                  ( EApplication
                      ()
                      ()
                      (EVariable () (Label () "trace_int32"))
                      ( EApplication
                          ()
                          ()
                          (EVariable () (Label () "factorial"))
                          ( EApplication
                              ()
                              ()
                              (EVariable () (Label () "from_int32"))
                              (ELiteral () (LInt32 12) :| [])
                              :| []
                          )
                          :| []
                      )
                  )
              ) ::
              Definition () Kind ()
          )
    it "" $ do
      runParser parseModule "" "module Main { import Utilities(factorial); fn main() = trace_int32(factorial(12)); }"
        == Right
          ( Module
              (Path ["Main"])
              ["*"]
              [ DImport (Path ["Utilities"]) ["factorial"]
              , DFunction
                  "main"
                  ( Function
                      ()
                      (With [] ())
                      (PLiteral () LUnit :| [])
                      ( EApplication
                          ()
                          ()
                          (EVariable () (Label () "trace_int32"))
                          ( EApplication
                              ()
                              ()
                              (EVariable () (Label () "factorial"))
                              ( EApplication
                                  ()
                                  ()
                                  (EVariable () (Label () "from_int32"))
                                  (ELiteral () (LInt32 12) :| [])
                                  :| []
                              )
                              :| []
                          )
                      )
                  )
              ] ::
              Module () Kind ()
          )
    it "" $ do
      runParser parseModule "" "module Utilities(factorial) { fn factorial(n : int32) : int32 = fold(pack_nat(n)) { | Zero => 1 | Succ(@f) as m => unpack_nat(m) * f }; }"
        == Right
          ( Module
              (Path ["Utilities"])
              ["factorial"]
              [ DAnnotation
                  (With [] (TIntrinsic IInt32))
                  ( DFunction
                      "factorial"
                      ( Function
                          ()
                          (With [] ())
                          ( PAnnotation
                              ()
                              (TIntrinsic IInt32)
                              (PVariable () (Label () "n"))
                              :| []
                          )
                          ( EFold
                              ()
                              ()
                              ( EApplication
                                  ()
                                  ()
                                  (EVariable () (Label () "pack_nat"))
                                  ( EVariable () (Label () "n")
                                      :| []
                                  )
                                  :| []
                              )
                              ( EClause
                                  ()
                                  ( PConstructor
                                      ()
                                      (Label () "Zero")
                                      []
                                  )
                                  ( CPlain
                                      ()
                                      []
                                      ( EApplication
                                          ()
                                          ()
                                          (EVariable () (Label () "from_int32"))
                                          (ELiteral () (LInt32 1) :| [])
                                      )
                                      :| []
                                  )
                                  <| EClause
                                    ()
                                    ( PAs
                                        ()
                                        (Label () "m")
                                        ( PConstructor
                                            ()
                                            (Label () "Succ")
                                            [ PAtVariable () (Label () "f")
                                            ]
                                        )
                                    )
                                    ( CPlain
                                        ()
                                        []
                                        ( EApplication
                                            ()
                                            ()
                                            (EBinaryOperator () () OMultiplication)
                                            ( EApplication
                                                ()
                                                ()
                                                (EVariable () (Label () "unpack_nat"))
                                                ( EVariable () (Label () "m")
                                                    :| []
                                                )
                                                <| EVariable () (Label () "f")
                                                :| []
                                            )
                                        )
                                        :| []
                                    )
                                  :| []
                              )
                              Nothing
                          )
                      )
                  ) ::
                  Definition () Kind ()
              ]
          )
    it "" $ do
      runParser parseFunctionDefinition "" "fn main() = let xs = [5, 3, 7, 2, 1, 6, 4] : list(int32) in trace_int32(match(sort(xs)) { | [] => 12345 | (y :: _) => y });"
        == Right
          ( DFunction
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
                                  [ EApplication
                                      ()
                                      ()
                                      (EVariable () (Label () "from_int32"))
                                      (ELiteral () (LInt32 5) :| [])
                                  , EApplication
                                      ()
                                      ()
                                      (EVariable () (Label () "from_int32"))
                                      (ELiteral () (LInt32 3) :| [])
                                  , EApplication
                                      ()
                                      ()
                                      (EVariable () (Label () "from_int32"))
                                      (ELiteral () (LInt32 7) :| [])
                                  , EApplication
                                      ()
                                      ()
                                      (EVariable () (Label () "from_int32"))
                                      (ELiteral () (LInt32 2) :| [])
                                  , EApplication
                                      ()
                                      ()
                                      (EVariable () (Label () "from_int32"))
                                      (ELiteral () (LInt32 1) :| [])
                                  , EApplication
                                      ()
                                      ()
                                      (EVariable () (Label () "from_int32"))
                                      (ELiteral () (LInt32 6) :| [])
                                  , EApplication
                                      ()
                                      ()
                                      (EVariable () (Label () "from_int32"))
                                      (ELiteral () (LInt32 4) :| [])
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
                                  ( PListLiteral () () []
                                  )
                                  ( CPlain
                                      ()
                                      []
                                      ( EApplication
                                          ()
                                          ()
                                          (EVariable () (Label () "from_int32"))
                                          (ELiteral () (LInt32 12345) :| [])
                                      )
                                      :| []
                                  )
                                  <| EClause
                                    ()
                                    ( PListCons
                                        ()
                                        ()
                                        (PVariable () (Label () "y"))
                                        (PAny () ())
                                    )
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
              ) ::
              Definition () Kind ()
          )
    it "" $ do
      runParser parseFunctionDefinition "" "fn flatten(tree) = fold(tree) { | Node(y, @lhs, @rhs) => lhs ++ (y :: rhs) | Leaf => [] };"
        == Right
          ( DFunction
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
              ) ::
              Definition () Kind ()
          )
    it "" $ do
      runParser parseConstantDefinition "" "sort = flatten << from_list;"
        == Right
          ( DConstant
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
              ) ::
              Definition () Kind ()
          )
    it "" $ do
      runParser parseFunctionDefinition "" "fn from_list(list) = fold(list, { min = 0, max = -1 }) { | (p :: @g) => fn(range) => if (p |. in_range(range)) then Node ( p , g({ min = range.min, max = p }) , g({ min = p, max = range.max })) else g(range) | [] => always(Leaf) };"
        == Right
          ( DFunction
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
              ) ::
              Definition () Kind ()
          )
    it "" $ do
      runParser parseFunctionDefinition "" "fn in_range(range, n) = greater_than(n, range.min) && (less_than_or_equal_to(n, range.max) || less_than_or_equal_to(range.max, -1));"
        == Right
          ( DFunction
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
              ) ::
              Definition () Kind ()
          )
    it "" $ do
      runParser parseFunctionDefinition "" "fn greater_than(n) = not << less_than_or_equal_to(n);"
        == Right
          ( DFunction
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
              ) ::
              Definition () Kind ()
          )
    it "" $ do
      runParser parseFunctionDefinition "" "fn less_than_or_equal_to(m) = fn(n) => match(compare(m, n)) { | LessThan or EqualTo => true | GreaterThan => false };"
        == Right
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
              ) ::
              Definition () Kind ()
          )
    it "" $ do
      runParser parseTypeDefinition "" "type Ordering = LessThan | EqualTo | GreaterThan"
        == Right
          ( DType
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
              ] ::
              Definition () Kind ()
          )
    it "" $ do
      runParser parseTypeDefinition "" "type Tree(a) = Node(a, Tree(a), Tree(a)) | Leaf"
        == Right
          ( DType
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
              ] ::
              Definition () Kind ()
          )
    it "" $ do
      runParser parseTraitDefinition "" "trait Ordered(a) { compare : a -> a -> Ordering }"
        == Right
          ( DTrait
              "Ordered"
              []
              (Parameter KType "a")
              [
                ( "compare"
                , TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TConstructor () "Ordering"
                )
              ] ::
              Definition () Kind ()
          )
    it "" $ do
      runParser parseTraitDefinition "" "trait Ordered(a : *) { compare : a -> a -> Ordering }"
        == Right
          ( DTrait
              "Ordered"
              []
              (Parameter KType "a")
              [
                ( "compare"
                , TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TConstructor () "Ordering"
                )
              ] ::
              Definition () Kind ()
          )
    it "" $ do
      runParser parseTraitInstance "" "instance Ordered(int32) { fn compare(x, y) = if (x < y) then LessThan else if (x > y) then GreaterThan else EqualTo; }"
        == Right
          ( DInstance
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
              ] ::
              Definition () Kind ()
          )
    it "" $ do
      runParser parseModule "" "module Main { import Core$(trace_int32, trace_bool, not, always, from_int32, `from_int32__$instance_Numeric(Intrinsic(Int32))`); type Ordering = LessThan | EqualTo | GreaterThan trait Ordered(a) { compare : a -> a -> Ordering } instance Ordered(int32) { fn compare(x, y) = if (x < y) then LessThan else if (x > y) then GreaterThan else EqualTo; } type Tree(a) = Node(a, Tree(a), Tree(a)) | Leaf fn less_than_or_equal_to(m) = fn(n) => match(compare(m, n)) { | LessThan or EqualTo => true | GreaterThan => false }; fn greater_than(n) = not << less_than_or_equal_to(n); fn in_range(range, n) = greater_than(n, range.min) && (less_than_or_equal_to(n, range.max) || less_than_or_equal_to(range.max, -1)); fn from_list(list) = fold(list, { min = 0, max = -1 }) { | (p :: @g) => fn(range) => if (p |. in_range(range)) then Node ( p , g({ min = range.min, max = p }) , g({ min = p, max = range.max })) else g(range) | [] => always(Leaf) }; fn flatten(tree) = fold(tree) { | Node(y, @lhs, @rhs) => lhs ++ (y :: rhs) | Leaf => [] }; sort = flatten << from_list; fn main() = let xs = [5, 3, 7, 2, 1, 6, 4] : list(int32) in trace_int32(match(sort(xs)) { | [] => 12345 | (y :: _) => y }); }"
        == Right moduleTest
    it "" $ do
      runParser parseModule "" "module Main { import Core$(trace_int32, trace_bool, not, always, from_int32, `from_int32__$instance_Numeric(Intrinsic(Int32))`); type Ordering = LessThan | EqualTo | GreaterThan trait Ordered(a) { compare : a -> a -> Ordering } instance Ordered(int32) { fn compare(x, y) = if (x < y) then LessThan else if (x > y) then GreaterThan else EqualTo; } type Tree(a) = Node(a, Tree(a), Tree(a)) | Leaf fn less_than_or_equal_to(m) = fn(n) => match(compare(m, n)) { | LessThan or EqualTo => true | GreaterThan => false }; fn greater_than(n) = not << less_than_or_equal_to(n); fn in_range(range, n) = greater_than(n, range.min) && (less_than_or_equal_to(n, range.max) || less_than_or_equal_to(range.max, -1)); fn from_list(list) = fold(list, { min = 0, max = -1 }) { | (p :: @g) => fn(range) => if (p |. in_range(range)) then Node ( p , g({ min = range.min, max = p }) , g({ min = p, max = range.max })) else g(range) | [] => always(Leaf) }; fn flatten(tree) = fold(tree) { | Node(y, @lhs, @rhs) => lhs ++ (y :: rhs) | Leaf => [] }; sort = flatten << from_list; fn main() = let xs = [5, 3, 7, 2, 1, 6, 4] : list(int32) in trace_int32(match(sort(xs)) { | [] => 12345 | (y :: _) => y }); }"
        == Right moduleTest2

-- ( Module.fromDefinitionList
--            (Path ["Main"])
--            ["*"]
--            [ DImport (Path ["Core$"]) ["trace_int32", "trace_bool", "not", "always", "from_int32", "from_int32__$instance_Numeric(Intrinsic(Int32))"]
--            , DType
--                "Ordering"
--                []
--                [ Constructor
--                    "LessThan"
--                    0
--                    (Forall mempty [] (TConstructor () "Ordering"))
--                , Constructor
--                    "EqualTo"
--                    0
--                    (Forall mempty [] (TConstructor () "Ordering"))
--                , Constructor
--                    "GreaterThan"
--                    0
--                    (Forall mempty [] (TConstructor () "Ordering"))
--                ]
--            , DTrait
--                "Ordered"
--                []
--                (TVariable (Parameter () "a"))
--                [
--                  ( "compare"
--                  , TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TConstructor () "Ordering"
--                  )
--                ]
--            , DInstance
--                "Ordered"
--                (TIntrinsic IInt32)
--                [ DFunction
--                    "compare"
--                    ( Function
--                        ()
--                        (With [] ())
--                        ( PVariable () (Label () "x")
--                            <| PVariable () (Label () "y")
--                            :| []
--                        )
--                        ( EIf
--                            ()
--                            ()
--                            ( EApplication
--                                ()
--                                ()
--                                (EBinaryOperator () () OLessThan)
--                                ( EVariable () (Label () "x")
--                                    <| EVariable () (Label () "y")
--                                    :| []
--                                )
--                            )
--                            (EConstructor () (Label () "LessThan"))
--                            ( EIf
--                                ()
--                                ()
--                                ( EApplication
--                                    ()
--                                    ()
--                                    (EBinaryOperator () () OGreaterThan)
--                                    ( EVariable () (Label () "x")
--                                        <| EVariable () (Label () "y")
--                                        :| []
--                                    )
--                                )
--                                (EConstructor () (Label () "GreaterThan"))
--                                (EConstructor () (Label () "EqualTo"))
--                            )
--                        )
--                    )
--                ]
--            , DType
--                "Tree"
--                [Parameter () "a"]
--                [ Constructor
--                    "Node"
--                    3
--                    ( Forall
--                        (Set.fromList [Parameter () "a"])
--                        []
--                        ( TVariable (Parameter () "a")
--                            `TArrow` TApplication () (TConstructor () "Tree") (TVariable (Parameter () "a") :| [])
--                            `TArrow` TApplication () (TConstructor () "Tree") (TVariable (Parameter () "a") :| [])
--                            `TArrow` TApplication () (TConstructor () "Tree") (TVariable (Parameter () "a") :| [])
--                        )
--                    )
--                , Constructor
--                    "Leaf"
--                    0
--                    ( Forall
--                        (Set.fromList [Parameter () "a"])
--                        []
--                        ( TApplication
--                            ()
--                            (TConstructor () "Tree")
--                            (TVariable (Parameter () "a") :| [])
--                        )
--                    )
--                ]
--            , DFunction
--                "less_than_or_equal_to"
--                ( Function
--                    ()
--                    (With [] ())
--                    (PVariable () (Label () "m") :| [])
--                    ( ELambda
--                        ()
--                        (PVariable () (Label () "n") :| [])
--                        ( EMatch
--                            ()
--                            ()
--                            ( EApplication
--                                ()
--                                ()
--                                (EVariable () (Label () "compare"))
--                                ( EVariable () (Label () "m")
--                                    <| EVariable () (Label () "n")
--                                    :| []
--                                )
--                            )
--                            ( EClause
--                                ()
--                                ( POr
--                                    ()
--                                    ()
--                                    (PConstructor () (Label () "LessThan") [])
--                                    (PConstructor () (Label () "EqualTo") [])
--                                )
--                                (CPlain () [] (ELiteral () (LBool True)) :| [])
--                                <| EClause
--                                  ()
--                                  (PConstructor () (Label () "GreaterThan") [])
--                                  (CPlain () [] (ELiteral () (LBool False)) :| [])
--                                :| []
--                            )
--                        )
--                    )
--                )
--            , DFunction
--                "greater_than"
--                ( Function
--                    ()
--                    (With [] ())
--                    (PVariable () (Label () "n") :| [])
--                    ( EApplication
--                        ()
--                        ()
--                        (EBinaryOperator () () OReverseComposition)
--                        ( EVariable () (Label () "not")
--                            <| EApplication
--                              ()
--                              ()
--                              (EVariable () (Label () "less_than_or_equal_to"))
--                              (EVariable () (Label () "n") :| [])
--                            :| []
--                        )
--                    )
--                )
--            , DFunction
--                "in_range"
--                ( Function
--                    ()
--                    (With [] ())
--                    ( PVariable () (Label () "range")
--                        <| PVariable () (Label () "n")
--                        :| []
--                    )
--                    ( EApplication
--                        ()
--                        ()
--                        (EBinaryOperator () () OLogicalAnd)
--                        ( EApplication
--                            ()
--                            ()
--                            (EVariable () (Label () "greater_than"))
--                            ( EVariable () (Label () "n")
--                                <| ESelect () (Label () "min") (EVariable () (Label () "range"))
--                                :| []
--                            )
--                            <| EApplication
--                              ()
--                              ()
--                              (EBinaryOperator () () OLogicalOr)
--                              ( EApplication
--                                  ()
--                                  ()
--                                  (EVariable () (Label () "less_than_or_equal_to"))
--                                  ( EVariable () (Label () "n")
--                                      <| ESelect () (Label () "max") (EVariable () (Label () "range"))
--                                      :| []
--                                  )
--                                  <| EApplication
--                                    ()
--                                    ()
--                                    (EVariable () (Label () "less_than_or_equal_to"))
--                                    ( ESelect () (Label () "max") (EVariable () (Label () "range"))
--                                        <| EApplication
--                                          ()
--                                          ()
--                                          (EVariable () (Label () "from_int32"))
--                                          ( ELiteral () (LInt32 (-1))
--                                              :| []
--                                          )
--                                        :| []
--                                    )
--                                  :| []
--                              )
--                            :| []
--                        )
--                    )
--                )
--            , DFunction
--                "from_list"
--                ( Function
--                    ()
--                    (With [] ())
--                    (PVariable () (Label () "list") :| [])
--                    ( EFold
--                        ()
--                        ()
--                        ( EVariable () (Label () "list")
--                            <| ERecord
--                              ()
--                              ()
--                              ( Map.fromList
--                                  [
--                                    ( "min"
--                                    , EApplication
--                                        ()
--                                        ()
--                                        (EVariable () (Label () "from_int32"))
--                                        ( ELiteral () (LInt32 0)
--                                            :| []
--                                        )
--                                    )
--                                  ,
--                                    ( "max"
--                                    , EApplication
--                                        ()
--                                        ()
--                                        (EVariable () (Label () "from_int32"))
--                                        ( ELiteral () (LInt32 (-1))
--                                            :| []
--                                        )
--                                    )
--                                  ]
--                              )
--                              Nothing
--                            :| []
--                        )
--                        ( EClause
--                            ()
--                            ( PListCons
--                                ()
--                                ()
--                                (PVariable () (Label () "p"))
--                                (PAtVariable () (Label () "g"))
--                            )
--                            ( CPlain
--                                ()
--                                []
--                                ( ELambda
--                                    ()
--                                    (PVariable () (Label () "range") :| [])
--                                    ( EIf
--                                        ()
--                                        ()
--                                        ( EApplication
--                                            ()
--                                            ()
--                                            (EBinaryOperator () () OReverseApplication)
--                                            ( EVariable () (Label () "p")
--                                                <| EApplication
--                                                  ()
--                                                  ()
--                                                  (EVariable () (Label () "in_range"))
--                                                  (EVariable () (Label () "range") :| [])
--                                                :| []
--                                            )
--                                        )
--                                        ( EApplication
--                                            ()
--                                            ()
--                                            (EConstructor () (Label () "Node"))
--                                            ( EVariable () (Label () "p")
--                                                <| EApplication
--                                                  ()
--                                                  ()
--                                                  (EVariable () (Label () "g"))
--                                                  ( ERecord
--                                                      ()
--                                                      ()
--                                                      ( Map.fromList
--                                                          [
--                                                            ( "min"
--                                                            , ESelect () (Label () "min") (EVariable () (Label () "range"))
--                                                            )
--                                                          ,
--                                                            ( "max"
--                                                            , EVariable () (Label () "p")
--                                                            )
--                                                          ]
--                                                      )
--                                                      Nothing
--                                                      :| []
--                                                  )
--                                                <| EApplication
--                                                  ()
--                                                  ()
--                                                  (EVariable () (Label () "g"))
--                                                  ( ERecord
--                                                      ()
--                                                      ()
--                                                      ( Map.fromList
--                                                          [
--                                                            ( "min"
--                                                            , EVariable () (Label () "p")
--                                                            )
--                                                          ,
--                                                            ( "max"
--                                                            , ESelect () (Label () "max") (EVariable () (Label () "range"))
--                                                            )
--                                                          ]
--                                                      )
--                                                      Nothing
--                                                      :| []
--                                                  )
--                                                :| []
--                                            )
--                                        )
--                                        (EApplication () () (EVariable () (Label () "g")) (EVariable () (Label () "range") :| []))
--                                    )
--                                )
--                                :| []
--                            )
--                            <| EClause
--                              ()
--                              (PListLiteral () () [])
--                              ( CPlain
--                                  ()
--                                  []
--                                  ( EApplication
--                                      ()
--                                      ()
--                                      (EVariable () (Label () "always"))
--                                      (EConstructor () (Label () "Leaf") :| [])
--                                  )
--                                  :| []
--                              )
--                            :| []
--                        )
--                        Nothing
--                    )
--                )
--            , DFunction
--                "flatten"
--                ( Function
--                    ()
--                    (With [] ())
--                    (PVariable () (Label () "tree") :| [])
--                    ( EFold
--                        ()
--                        ()
--                        (EVariable () (Label () "tree") :| [])
--                        ( EClause
--                            ()
--                            ( PConstructor
--                                ()
--                                (Label () "Node")
--                                [ PVariable () (Label () "y")
--                                , PAtVariable () (Label () "lhs")
--                                , PAtVariable () (Label () "rhs")
--                                ]
--                            )
--                            ( CPlain
--                                ()
--                                []
--                                ( EApplication
--                                    ()
--                                    ()
--                                    (EBinaryOperator () () OListConcatenation)
--                                    ( EVariable () (Label () "lhs")
--                                        <| EListCons () () (EVariable () (Label () "y")) (EVariable () (Label () "rhs"))
--                                        :| []
--                                    )
--                                )
--                                :| []
--                            )
--                            <| EClause
--                              ()
--                              (PConstructor () (Label () "Leaf") [])
--                              ( CPlain
--                                  ()
--                                  []
--                                  (EListLiteral () () [])
--                                  :| []
--                              )
--                            :| []
--                        )
--                        Nothing
--                    )
--                )
--            , DConstant
--                "sort"
--                ( Constant
--                    ()
--                    (With [] ())
--                    ( EApplication
--                        ()
--                        ()
--                        (EBinaryOperator () () OReverseComposition)
--                        ( EVariable () (Label () "flatten")
--                            <| EVariable () (Label () "from_list")
--                            :| []
--                        )
--                    )
--                )
--            , DFunction
--                "main"
--                ( Function
--                    ()
--                    (With [] ())
--                    (PLiteral () LUnit :| [])
--                    ( ELet
--                        ()
--                        ( BPattern
--                            ()
--                            (PVariable () (Label () "xs"))
--                            ( EAnnotation
--                                ()
--                                (TIntrinsic (IList (TIntrinsic IInt32)))
--                                ( EListLiteral
--                                    ()
--                                    ()
--                                    [ EApplication () () (EVariable () (Label () "from_int32")) (ELiteral () (LInt32 5) :| []) -- ELiteral () (LInt32 5)
--                                    , EApplication () () (EVariable () (Label () "from_int32")) (ELiteral () (LInt32 3) :| []) -- Literal () (LInt32 3)
--                                    , EApplication () () (EVariable () (Label () "from_int32")) (ELiteral () (LInt32 7) :| []) -- Literal () (LInt32 7)
--                                    , EApplication () () (EVariable () (Label () "from_int32")) (ELiteral () (LInt32 2) :| []) -- Literal () (LInt32 2)
--                                    , EApplication () () (EVariable () (Label () "from_int32")) (ELiteral () (LInt32 1) :| []) -- Literal () (LInt32 1)
--                                    , EApplication () () (EVariable () (Label () "from_int32")) (ELiteral () (LInt32 6) :| []) -- Literal () (LInt32 6)
--                                    , EApplication () () (EVariable () (Label () "from_int32")) (ELiteral () (LInt32 4) :| []) -- Literal () (LInt32 4)
--                                    ]
--                                )
--                            )
--                            :| []
--                        )
--                        ( EApplication
--                            ()
--                            ()
--                            (EVariable () (Label () "trace_int32"))
--                            ( EMatch
--                                ()
--                                ()
--                                ( EApplication
--                                    ()
--                                    ()
--                                    (EVariable () (Label () "sort"))
--                                    (EVariable () (Label () "xs") :| [])
--                                )
--                                ( EClause
--                                    ()
--                                    (PListLiteral () () [])
--                                    ( CPlain
--                                        ()
--                                        []
--                                        (ELiteral () (LInt32 12445))
--                                        :| []
--                                    )
--                                    <| EClause
--                                      ()
--                                      (PListCons () () (PVariable () (Label () "y")) (PAny () ()))
--                                      ( CPlain
--                                          ()
--                                          []
--                                          (EVariable () (Label () "y"))
--                                          :| []
--                                      )
--                                    :| []
--                                )
--                                :| []
--                            )
--                        )
--                    )
--                )
--            ]
--            )

moduleTest =
  ( Module.fromDefinitionList
      (Path ["Main"])
      ["*"]
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
          (Parameter KType "a")
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
                                  ( EApplication
                                      ()
                                      ()
                                      (EVariable () (Label () "from_int32"))
                                      (ELiteral () (LInt32 12345) :| [])
                                  )
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
  )

moduleTest2 =
  Module.fromDefinitionList
    (Path ["Main"])
    -- Exports
    ["*"]
    -- Definitions
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
        (Parameter KType "a")
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
                                ( EApplication
                                    ()
                                    ()
                                    (EVariable () (Label () "from_int32"))
                                    (ELiteral () (LInt32 12345) :| [])
                                )
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
