{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystemExamples.Test08 where

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
  describe "let qsort = flatten << from_list in qsort" $
    it "" $ do
      testResultExpression (runTest fixture) == fixture1

runTest :: (Show a, Eq a, Data a) => Expression a () -> TestResult (Expression a (Type TypeIndex Kind)) a
runTest =
  runTypedExpressionTest
    (CompilerEnvironment env1 env2 env3)
    [
      ( "flatten"
      , Forall
          (Set.fromList [TypeIndex KType 0])
          []
          (tree0Type `TArrow` list0Type)
      )
    ,
      ( "from_list"
      , Forall
          (Set.fromList [TypeIndex KType 0])
          []
          (list0Type `TArrow` tree0Type)
      )
    ]
 where
  env1 =
    Environment.fromList
      []
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
-- let
--   qsort =
--     flatten << from_list
--   in
--     qsort
--
fixture :: Expression () ()
fixture =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label () "qsort"))
        ( EApplication
            ()
            ()
            (EBinaryOperator () () OReverseComposition)
            (EVariable () (Label () "flatten") <| EVariable () (Label () "from_list") :| [])
        )
        :| []
    )
    (EVariable () (Label () "qsort"))

fixture1 :: Expression () (Type TypeIndex Kind)
fixture1 =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label (list0Type `TArrow` list0Type) "qsort"))
        ( EApplication
            ()
            (list0Type `TArrow` list0Type)
            ( EBinaryOperator
                ()
                ( (tree0Type `TArrow` list0Type)
                    `TArrow` (list0Type `TArrow` tree0Type)
                    `TArrow` list0Type
                    `TArrow` list0Type
                )
                OReverseComposition
            )
            ( EVariable () (Label (tree0Type `TArrow` list0Type) "flatten")
                <| EVariable () (Label (list0Type `TArrow` tree0Type) "from_list")
                  :| []
            )
        )
        :| []
    )
    (EVariable () (Label (listType 1 `TArrow` listType 1) "qsort"))
