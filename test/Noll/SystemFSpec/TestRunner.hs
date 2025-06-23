{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.SystemFSpec.TestRunner where

import Control.Monad.Identity (runIdentity)
import Control.Monad.State (get, gets)
import Data.Data (Data)
import Data.List.NonEmpty ((<|))
import Debug.Trace
import Lang.Common.Environment (Environment)
import Lang.Common.List1 (NonEmpty (..))
import Lang.Utils (Name, forM_)
import Noll.Compiler
import Noll.Language
import Noll.Module (Constant (..), Function (..), Module (..))
import Noll.SystemF.Constraint.Assumption (Assumption (..))
import Noll.SystemF.Constraint.Generation (ConstraintsGenError)
import Noll.SystemF.Constraint.Generation.Internal (InferenceRule (..))
import Noll.SystemF.Substitution (apply, normalizeTypeIndexes)

import qualified Data.Set as Set
import qualified Lang.Common.Environment as Environment

data TestResult r a = TestResult
  { testResultExpression :: r
  , testResultAssumptions :: [Assumption IndexedType]
  , testResultErrors1 :: [ConstraintsGenError a]
  , testResultErrors2 :: [InferenceRule Kind a]
  }
  deriving (Show, Eq, Ord)

runTypedConstantTest ::
  (Show a, Eq a, Data a) =>
  CompilerEnvironment ->
  [(Name, Scheme TypeIndex Kind IndexedType)] ->
  Constant Expression a t ->
  TestResult (Constant Expression a (Type TypeIndex Kind)) a
runTypedConstantTest env names g =
  undefined

--  runIdentity $ evalCompilerT env $ do
--    insertNamesC names
--    g1 <- indexedC g
--    (g2, as) <- typeCheckConstantC g1
--    errs0 <- getConstraintsGenErrorsC
--    errs1 <- getSolverRuleViolationsC
--    pure (TestResult (normalizeTypeIndexes g2) as errs0 errs1)

runTypedFunctionTest ::
  (Show a, Eq a, Data a) =>
  CompilerEnvironment ->
  [(Name, Scheme TypeIndex Kind IndexedType)] ->
  Function Expression a t ->
  TestResult (Function Expression a (Type TypeIndex Kind)) a
runTypedFunctionTest env names f =
  undefined

--  runIdentity $ evalCompilerT env $ do
--    insertNamesC names
--    f1 <- indexedC f
----    traceShowM "vvvvvvvvvvvvvvvvvv"
----    traceShowM f1
--    (f2, as) <- typeCheckFunctionC f1
--    errs0 <- getConstraintsGenErrorsC
--    errs1 <- getSolverRuleViolationsC
--    pure (TestResult (normalizeTypeIndexes f2) as errs0 errs1)

-- runTypedDefinitionsTest ::
--  ( Show a
--  , Eq a
--  , Show k
--  , HasType TypeIndex Kind (Definition a k (Type TypeIndex Kind))
--  , TypeIndexed Kind (Definition a k IndexedType)
--  ) =>
--  CompilerEnvironment ->
--  [(Name, Scheme TypeIndex Kind IndexedType)] ->
--  [Definition a k t] ->
--  TestResult [Definition a Kind IndexedType] a
runTypedDefinitionsTest env names ds =
  runIdentity $ evalCompilerT env $ do
    insertNamesC names
    ds1 <- traverse indexedC ds
    (ds2, as) <- typeCheckDefinitionsC ds1
    errs0 <- getConstraintsGenErrorsC
    errs1 <- getSolverRuleViolationsC
    pure (TestResult (normalizeTypeIndexes ds2) as errs0 errs1)

-- runTypedModuleTest ::
--  (Show a, Eq a) =>
--  CompilerEnvironment ->
--  [(Name, Scheme TypeIndex Kind IndexedType)] ->
--  Module a k t ->
--  TestResult (Module a Kind (Type TypeIndex Kind)) a
runTypedModuleTest env names (Module p ns ds) = TestResult (Module p ns a) b c d
 where
  TestResult a b c d = runTypedDefinitionsTest env names ds

typeCheckExpressionC e = do
  compileConstraintsC e
  ams <- gets compilerAssumptions
  -- sub <- gets compilerSubstitution
  sub <- solveC
  pure (normalizeRowTypes <$> apply sub e, apply sub ams)

runTypedExpressionTest ::
  (Show a, Eq a, Data a) =>
  CompilerEnvironment ->
  [(Name, Scheme TypeIndex Kind IndexedType)] ->
  Expression a t ->
  TestResult (Expression a (Type TypeIndex Kind)) a
runTypedExpressionTest env names e =
  runIdentity $ evalCompilerT env $ do
    insertNamesC names
    e1 <- indexedC e
    (e2, as) <- typeCheckExpressionC e1
    errs0 <- getConstraintsGenErrorsC
    errs1 <- getSolverRuleViolationsC
    pure (TestResult (normalizeTypeIndexes e2) as errs0 errs1)

testRunner :: (CompilerEnvironment -> t) -> t
testRunner f = f (CompilerEnvironment testDataConstructorEnv testTypeConstructorEnv testTraitEnvironment)

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

testTraitEnvironment =
  Environment.fromList
    []

gork123 =
  123

newtype DictShow d a = DictShow {show_ :: d -> a -> String}

unpackShow :: DictShow d a -> d -> a -> String
unpackShow (DictShow show_) = show_

instanceShowString :: DictShow () String
instanceShowString = DictShow{show_ = \_ s -> s}

instanceShowInt :: DictShow () Int
instanceShowInt = DictShow{show_ = \_ n -> show n}

instanceShowTuple = DictShow{show_ = \(d1, d2) (a, b) -> unpackShow d1 () a <> "," <> unpackShow d2 () b}

instanceShowTuple1 = DictShow{show_ = \(d1, d2) (a, b) -> unpackShow d1 () a <> "," <> unpackShow d2 () b}

instanceShowList = DictShow{show_ = \d1 (x : _) -> "[" <> unpackShow d1 () x <> "]"}

exampleFoo =
  let p = (1, "hello") :: (Int, String)
   in unpackShow instanceShowTuple (instanceShowInt, instanceShowString) p

exampleBaz d1 d2 x y = unpackShow instanceShowTuple (d1, d2) (x, y)

exampleBar :: DictShow () a -> DictShow () b -> [(a, b)] -> String
exampleBar d1 d2 =
  unpackShow
    instanceShowList
    (DictShow{show_ = \() -> unpackShow instanceShowTuple (d1, d2)})

example1 = exampleBar instanceShowInt instanceShowString [(1, "foo")]
