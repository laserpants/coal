{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystemSpec.TestRunner where

import Data.List.NonEmpty ((<|))
import Debug.Trace
import Noll.Common.Environment (Environment)
import Noll.Common.List1 (NonEmpty (..))
import Noll.Compiler (
  CompilerEnvironment (..),
  evalCompiler,
  generateConstraintsC,
  getConstraintsGenerationErrorsC,
  getSolverRuleViolationsC,
  insertNamesC,
  solveConstraintsC,
  typeCheckExpressionC,
  typeCheckFunctionC,
  typeCheckGlobalC,
 )
import Noll.Language (
  Constructor (..),
  Expression (..),
  Function (..),
  Global (..),
  IndexedType,
  Intrinsic (..),
  Kind (..),
  Scheme (..),
  Type (..),
  TypeIndex (..),
  indexed,
 )
import Noll.TypeSystem.Constraint.Assumption (Assumption (..))
import Noll.TypeSystem.Constraint.Generation (ConstraintsGenerationError)
import Noll.TypeSystem.Constraint.Generation.Internal (InferenceRule (..))
import Noll.TypeSystem.Substitution (apply, normalizeTypeIndexes)
import Noll.Utils (Name)

import qualified Data.Set as Set
import qualified Noll.Common.Environment as Environment

data TestResult r a = TestResult
  { testResultExpression :: r
  , testResultAssumptions :: [Assumption IndexedType]
  , testResultErrors1 :: [ConstraintsGenerationError a]
  , testResultErrors2 :: [InferenceRule Kind a]
  }
  deriving (Show, Eq, Ord)

runTypedGlobalTest ::
  (Show a, Eq a) =>
  CompilerEnvironment ->
  [(Name, Scheme TypeIndex Kind IndexedType)] ->
  Global Expression a () ->
  TestResult (Global Expression a (Type TypeIndex Kind)) a
runTypedGlobalTest env names g =
  evalCompiler env $ do
    insertNamesC names
    (g2, as) <- typeCheckGlobalC (indexed g)
    errs0 <- getConstraintsGenerationErrorsC
    errs1 <- getSolverRuleViolationsC
    pure (TestResult g2 as errs0 errs1)

runTypedFunctionTest ::
  (Show a, Eq a) =>
  CompilerEnvironment ->
  [(Name, Scheme TypeIndex Kind IndexedType)] ->
  Function Expression a () ->
  TestResult (Function Expression a (Type TypeIndex Kind)) a
runTypedFunctionTest env names f =
  evalCompiler env $ do
    insertNamesC names
    (f2, as) <- typeCheckFunctionC (indexed f)
    errs0 <- getConstraintsGenerationErrorsC
    errs1 <- getSolverRuleViolationsC
    pure (TestResult f2 as errs0 errs1)

runTypedExpressionTest ::
  (Show a, Eq a) =>
  CompilerEnvironment ->
  [(Name, Scheme TypeIndex Kind IndexedType)] ->
  Expression a () ->
  TestResult (Expression a (Type TypeIndex Kind)) a
runTypedExpressionTest env names e =
  evalCompiler env $ do
    insertNamesC names
    (e2, as) <- typeCheckExpressionC (indexed e)
    errs0 <- getConstraintsGenerationErrorsC
    errs1 <- getSolverRuleViolationsC
    pure (TestResult e2 as errs0 errs1)

testRunner :: (Show a, Eq a) => [(Name, Scheme TypeIndex Kind IndexedType)] -> Expression a () -> TestResult (Expression a (Type TypeIndex Kind)) a
testRunner = runTypedExpressionTest (CompilerEnvironment testDataConstructorEnv testTypeConstructorEnv)

testDataConstructorEnv :: Environment (Constructor TypeIndex Kind (Type TypeIndex Kind))
testDataConstructorEnv =
  Environment.fromList
    [
      ( "Yes"
      , Constructor "Yes" 0 (Forall mempty [] (TConstructor KType "Answer"))
      )
    ,
      ( "No"
      , Constructor "No" 0 (Forall mempty [] (TConstructor KType "Answer"))
      )
    ,
      ( "Foo"
      , Constructor "Foo" 0 (Forall mempty [] (TConstructor KType "Foo"))
      )
    ,
      ( "Id"
      , Constructor
          "Id"
          1
          ( Forall (Set.fromList [TypeIndex KType 0]) [] (TVariable (TypeIndex KType 0) `TArrow` TApplication KType (TConstructor (KArrow KType KType) "Id") (TVariable (TypeIndex KType 0) :| []))
          )
      )
    ,
      ( "MkPair1"
      , Constructor
          "MkPair1"
          2
          ( Forall
              (Set.fromList [TypeIndex KType 0])
              []
              ( TVariable (TypeIndex KType 0)
                  `TArrow` TVariable (TypeIndex KType 0)
                  `TArrow` TApplication -- '0 -> Pair1('0)
                    KType
                    (TConstructor (KArrow KType KType) "Pair1")
                    (TVariable (TypeIndex KType 0) :| [])
              )
          )
      )
    ,
      ( "MkPair"
      , Constructor
          "MkPair"
          2
          ( Forall
              (Set.fromList [TypeIndex KType 0, TypeIndex KType 1])
              []
              ( TVariable (TypeIndex KType 0)
                  `TArrow` TVariable (TypeIndex KType 1)
                  `TArrow` TApplication -- '0 -> '1 -> Pair('0, '1)
                    KType
                    (TConstructor (KArrow KType (KArrow KType KType)) "Pair")
                    ( TVariable (TypeIndex KType 0)
                        <| TVariable (TypeIndex KType 1)
                          :| []
                    )
              )
          )
      )
    ,
      ( "MkIntPair"
      , Constructor
          "MkIntPair"
          2
          ( Forall
              mempty
              []
              (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TConstructor KType "IntPair")
          )
      )
    ]

testTypeConstructorEnv :: Environment Kind
testTypeConstructorEnv =
  Environment.fromList
    [ ("Answer", KType)
    , ("Pair1", KArrow KType KType) -- Homogeneous pair type
    , ("Pair", KArrow KType (KArrow KType KType))
    , ("IntPair", KType)
    , ("Id", KArrow KType KType)
    ]
