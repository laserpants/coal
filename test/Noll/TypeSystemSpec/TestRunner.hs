{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystemSpec.TestRunner where

import Data.List.NonEmpty ((<|))
import Debug.Trace
import Noll.Compiler (
  CompilerEnvironment (..),
  evalCompiler,
  generateConstraintsC,
  getConstraintsGenerationErrorsC,
  getSolverRuleViolationsC,
  solveConstraintsC,
 )
import Noll.Language (
  Constructor (..),
  Expression (..),
  IndexedType,
  Intrinsic (..),
  Kind (..),
  Scheme (..),
  Type (..),
  TypeIndex (..),
  indexed,
 )
import Noll.Lib.Environment (Environment)
import Noll.Lib.List1 (NonEmpty (..))
import Noll.TypeSystem.Constraint.Assumption (Assumption (..))
import Noll.TypeSystem.Constraint.Generation (ConstraintsGenerationError)
import Noll.TypeSystem.Constraint.Generation.Internal (InferenceRule (..))
import Noll.TypeSystem.Substitution (apply, normalizeTypeIndexes)

import qualified Data.Set as Set
import qualified Noll.Lib.Environment as Environment

testRunner ::
  (Eq a) =>
  Expression a () ->
  ( Expression a (Type TypeIndex Kind)
  , [Assumption IndexedType]
  , [ConstraintsGenerationError a]
  , [InferenceRule Kind a]
  )
testRunner e =
  evalCompiler (CompilerEnvironment testDataConstructorEnv testTypeConstructorEnv) $ do
    let e1 = indexed e
    (as, cs) <- generateConstraintsC e1
    sub <- solveConstraintsC cs
    errs0 <- getConstraintsGenerationErrorsC
    errs1 <- getSolverRuleViolationsC
    pure
      ( normalizeTypeIndexes (apply sub e1)
      , apply sub as
      , errs0
      , errs1
      )

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
