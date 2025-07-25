{-# LANGUAGE OverloadedStrings #-}

module Coal.TypeSystemExamples.Test13 where

import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Coal.Common.Label (Label (..))
import Coal.Compiler2
import Coal.Language (
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
  With (..),
 )
import Coal.Language.Module (Constant (..), Function (..), Module (..))
import Coal.TypeSystemSpec.TestRunner
import Test.Hspec (Spec, describe, hspec, it)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Coal.Common.Environment as Environment

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

tvariable1 :: IndexedType
tvariable1 = tvariable 1

list1Type :: IndexedType
list1Type = listType 1

tree1Type :: IndexedType
tree1Type = treeType 1

maxMin1Type :: IndexedType
maxMin1Type = maxMinType 1

spec :: Spec
spec =
  describe "" $
    it "" $ do
      testResultExpression (runTest fixture)
        == fixture1

runTest :: (Show a, Eq a, Data a) => Function Expression a () -> TestResult (Function Expression a (Type TypeIndex Kind)) a
runTest =
  runTypedFunctionTest
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
--  flatten(tree : Tree(a)) =>
--    fold(tree) {
--      | Node(y, @lhs, @rhs) =>
--          lhs ++ (y :: rhs)
--      | Leaf =>
--          []
--    }
--
fixture :: Function Expression () ()
fixture =
  Function
    ()
    (With [] ())
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

fixture1 :: Function Expression () (Type TypeIndex Kind)
fixture1 =
  Function
    ()
    (With [] (list1Type))
    (PAnnotation () (TApplication () (TConstructor () "Tree") (TVariable (Parameter () "a") :| [])) (PVariable () (Label tree1Type "tree")) :| [])
    ( EFold
        ()
        list1Type
        (EVariable () (Label tree1Type "tree") :| [])
        ( EClause
            ()
            ( PConstructor
                ()
                (Label tree1Type "Node")
                [ PVariable () (Label tvariable1 "y")
                , PAtVariable () (Label tree1Type "lhs")
                , PAtVariable () (Label tree1Type "rhs")
                ]
            )
            ( CPlain
                ()
                []
                ( EApplication
                    ()
                    list1Type
                    (EBinaryOperator () (list1Type `TArrow` list1Type `TArrow` list1Type) OListConcatenation)
                    ( EVariable () (Label list1Type "lhs")
                        <| EListCons () list1Type (EVariable () (Label tvariable1 "y")) (EVariable () (Label list1Type "rhs"))
                          :| []
                    )
                )
                :| []
            )
            <| EClause
              ()
              (PConstructor () (Label tree1Type "Leaf") [])
              ( CPlain
                  ()
                  []
                  (EListLiteral () list1Type [])
                  :| []
              )
              :| []
        )
        ( Just
            ( ERecursiveLet
                ()
                (PVariable () (Label (tree0Type `TArrow` list0Type) "$fold:1"))
                ( ELambda
                    ()
                    (PVariable () (Label (tree0Type) "$fold:1:expr") :| [])
                    ( EMatch
                        ()
                        (list0Type)
                        (EVariable () (Label (tree0Type) "$fold:1:expr"))
                        ( EClause
                            ()
                            ( PConstructor
                                ()
                                (Label (tree0Type) "Node")
                                [ PVariable () (Label (tvariable0) "y")
                                , PVariable () (Label (tree0Type) "lhs")
                                , PVariable () (Label (tree0Type) "rhs")
                                ]
                            )
                            ( CPlain
                                ()
                                []
                                ( EApplication
                                    ()
                                    list0Type
                                    ( EBinaryOperator
                                        ()
                                        (list0Type `TArrow` list0Type `TArrow` list0Type)
                                        OListConcatenation
                                    )
                                    ( EApplication
                                        ()
                                        list0Type
                                        (EVariable () (Label (tree0Type `TArrow` list0Type) "$fold:1"))
                                        (EVariable () (Label (tree0Type) "lhs") :| [])
                                        <| EListCons
                                          ()
                                          list0Type
                                          (EVariable () (Label (tvariable0) "y"))
                                          ( EApplication
                                              ()
                                              (list0Type)
                                              (EVariable () (Label (tree0Type `TArrow` list0Type) "$fold:1"))
                                              (EVariable () (Label (tree0Type) "rhs") :| [])
                                          )
                                          :| []
                                    )
                                )
                                :| []
                            )
                            <| EClause
                              ()
                              (PConstructor () (Label (tree0Type) "Leaf") [])
                              (CPlain () [] (EListLiteral () (list0Type) []) :| [])
                              :| []
                        )
                    )
                )
                ( EApplication
                    ()
                    (listType 1)
                    (EVariable () (Label (treeType 1 `TArrow` listType 1) "$fold:1"))
                    (EVariable () (Label (treeType 1) "tree") :| [])
                )
            )
        )
    )
