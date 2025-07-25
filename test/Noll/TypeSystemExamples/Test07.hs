{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystemExamples.Test07 where

import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Noll.Common.Label (Label (..))
import Noll.Compiler2
import Noll.Language (
  BinaryOperator (..),
  Binding (..),
  Choice (..),
  Clause (..),
  Constructor (..),
  Expression (..),
  IndexedType,
  Intrinsic (..),
  Kind (..),
  Parameter (..),
  Pattern (..),
  Primitive (..),
  Row (..),
  Scheme (..),
  Type (..),
  TypeIndex (..),
 )
import Noll.TypeSystemSpec.TestRunner
import Test.Hspec (Spec, describe, hspec, it)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Noll.Common.Environment as Environment

tvariable :: Int -> IndexedType
tvariable n = TVariable (TypeIndex KType n)

listType :: Int -> IndexedType
listType n = TIntrinsic (IList (tvariable n))

treeType :: Int -> IndexedType
treeType n = TApplication KType (TConstructor (KArrow KType KType) "Tree") (tvariable n :| [])

maxMinType :: Int -> IndexedType
maxMinType n = TIntrinsic (IRecord (TRow (RExtend "max" (tvariable n) (RExtend "min" (tvariable n) RNil))))

bool :: IndexedType
bool = TIntrinsic IBool

int32 :: IndexedType
int32 = TIntrinsic IInt32

tvariable0 :: IndexedType
tvariable0 = tvariable 0

list0Type :: IndexedType
list0Type = listType 0

tree0Type :: IndexedType
tree0Type = treeType 0

maxMin0Type :: IndexedType
maxMin0Type = maxMinType 0

spec :: Spec
spec =
  describe "let flatten = fn(tree : Tree(a)) => fold(tree) { | Node(y, @lhs, @rhs) => lhs ++ (y :: rhs) | Leaf => [] } in flatten" $
    it "" $ do
      testResultExpression (runTest fixture) == fixture1

runTest :: (Show a, Eq a, Data a) => Expression a () -> TestResult (Expression a (Type TypeIndex Kind)) a
runTest =
  runTypedExpressionTest
    (CompilerEnvironment env1 env2 env3)
    []
 where
  env1 =
    Environment.fromList
      [
        ( "Leaf"
        , Constructor
            "Leaf"
            0
            ( Forall
                (Set.fromList [TypeIndex KType 0])
                []
                ( TApplication
                    KType
                    (TConstructor (KArrow KType KType) "Tree")
                    (TVariable (TypeIndex KType 0) :| [])
                )
            )
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
                    `TArrow` ( TApplication
                                KType
                                (TConstructor (KArrow KType KType) "Tree")
                                (TVariable (TypeIndex KType 0) :| [])
                             )
                    `TArrow` ( TApplication
                                KType
                                (TConstructor (KArrow KType KType) "Tree")
                                (TVariable (TypeIndex KType 0) :| [])
                             )
                    `TArrow` ( TApplication
                                KType
                                (TConstructor (KArrow KType KType) "Tree")
                                (TVariable (TypeIndex KType 0) :| [])
                             )
                )
            )
        )
      ]
  env2 =
    Environment.fromList
      [ ("Ordering", KType)
      ,
        ( "Tree"
        , KArrow KType KType
        )
      ]
  env3 =
    Environment.fromList
      []

--
--  let
--    flatten =
--      fn(tree : Tree(a)) =>
--        fold(tree) {
--          | Node(y, @lhs, @rhs) =>
--              lhs ++ (y :: rhs)
--          | Leaf =>
--              []
--        }
--    in
--      flatten
--
fixture :: Expression () ()
fixture =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label () "flatten"))
        ( ELambda
            ()
            (PAnnotation () (TApplication () (TConstructor () "Tree") (TVariable (Parameter () "a") :| [])) (PVariable () (Label () "tree")) :| [])
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
                ( Just
                    ( ERecursiveLet
                        ()
                        (PVariable () (Label () "$fold:1"))
                        ( ELambda
                            ()
                            (PVariable () (Label () "$fold:1:expr") :| [])
                            ( EMatch
                                ()
                                ()
                                (EVariable () (Label () "$fold:1:expr"))
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
                                                ()
                                                OListConcatenation
                                            )
                                            ( EApplication
                                                ()
                                                ()
                                                (EVariable () (Label () "$fold:1"))
                                                (EVariable () (Label () "lhs") :| [])
                                                <| EListCons
                                                  ()
                                                  ()
                                                  (EVariable () (Label () "y"))
                                                  ( EApplication
                                                      ()
                                                      ()
                                                      (EVariable () (Label () "$fold:1"))
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
                            (EVariable () (Label () "$fold:1"))
                            (EVariable () (Label () "tree") :| [])
                        )
                    )
                )
            )
        )
        :| []
    )
    (EVariable () (Label () "flatten"))

fixture1 :: Expression () (Type TypeIndex Kind)
fixture1 =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label (tree0Type `TArrow` list0Type) "flatten"))
        ( ELambda
            ()
            (PAnnotation () (TApplication () (TConstructor () "Tree") (TVariable (Parameter () "a") :| [])) (PVariable () (Label tree0Type "tree")) :| [])
            ( EFold
                ()
                list0Type
                (EVariable () (Label tree0Type "tree") :| [])
                ( EClause
                    ()
                    ( PConstructor
                        ()
                        (Label tree0Type "Node")
                        [ PVariable () (Label tvariable0 "y")
                        , PAtVariable () (Label tree0Type "lhs")
                        , PAtVariable () (Label tree0Type "rhs")
                        ]
                    )
                    ( CPlain
                        ()
                        []
                        ( EApplication
                            ()
                            list0Type
                            (EBinaryOperator () (list0Type `TArrow` list0Type `TArrow` list0Type) OListConcatenation)
                            ( EVariable () (Label list0Type "lhs")
                                <| EListCons () list0Type (EVariable () (Label tvariable0 "y")) (EVariable () (Label list0Type "rhs"))
                                  :| []
                            )
                        )
                        :| []
                    )
                    <| EClause
                      ()
                      (PConstructor () (Label tree0Type "Leaf") [])
                      ( CPlain
                          ()
                          []
                          (EListLiteral () list0Type [])
                          :| []
                      )
                      :| []
                )
                ( Just
                    ( ERecursiveLet
                        ()
                        (PVariable () (Label (treeType 1 `TArrow` listType 1) "$fold:1"))
                        ( ELambda
                            ()
                            (PVariable () (Label (treeType 1) "$fold:1:expr") :| [])
                            ( EMatch
                                ()
                                (listType 1)
                                (EVariable () (Label (treeType 1) "$fold:1:expr"))
                                ( EClause
                                    ()
                                    ( PConstructor
                                        ()
                                        (Label (treeType 1) "Node")
                                        [ PVariable () (Label (tvariable 1) "y")
                                        , PVariable () (Label (treeType 1) "lhs")
                                        , PVariable () (Label (treeType 1) "rhs")
                                        ]
                                    )
                                    ( CPlain
                                        ()
                                        []
                                        ( EApplication
                                            ()
                                            (listType 1)
                                            ( EBinaryOperator
                                                ()
                                                ( listType 1 `TArrow` listType 1 `TArrow` listType 1
                                                )
                                                OListConcatenation
                                            )
                                            ( EApplication
                                                ()
                                                (listType 1)
                                                (EVariable () (Label (treeType 1 `TArrow` listType 1) "$fold:1"))
                                                (EVariable () (Label (treeType 1) "lhs") :| [])
                                                <| EListCons
                                                  ()
                                                  (listType 1)
                                                  (EVariable () (Label (tvariable 1) "y"))
                                                  ( EApplication
                                                      ()
                                                      (listType 1)
                                                      (EVariable () (Label (treeType 1 `TArrow` listType 1) "$fold:1"))
                                                      (EVariable () (Label (treeType 1) "rhs") :| [])
                                                  )
                                                  :| []
                                            )
                                        )
                                        :| []
                                    )
                                    <| EClause
                                      ()
                                      (PConstructor () (Label (treeType 1) "Leaf") [])
                                      (CPlain () [] (EListLiteral () (listType 1) []) :| [])
                                      :| []
                                )
                            )
                        )
                        ( EApplication
                            ()
                            (listType 0)
                            (EVariable () (Label (treeType 0 `TArrow` listType 0) "$fold:1"))
                            (EVariable () (Label (treeType 0) "tree") :| [])
                        )
                    )
                )
            )
        )
        :| []
    )
    (EVariable () (Label (treeType 2 `TArrow` listType 2) "flatten"))
