{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler (
  CompilerEnvironment (..),
  insertNamesC,
  Compiler (..),
  CompilerState (..),
  runCompiler,
  evalCompiler,
  runConstraintsGenerationC,
  generateConstraintsC,
  typedExpressionC,
  typedFunctionC,
  typedGlobalC,
  solveConstraintsC,
  getConstraintsGenerationErrorsC,
  getSolverRuleViolationsC,
) where

import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (MonadState, State, gets, modify, runState)
import Control.Monad.Writer (execWriter)
import Data.Either.Extra (partitionEithers)
import Debug.Trace
import Noll.Common.Environment (Environment (..))
import Noll.Language (
  Constructor (..),
  Expression (..),
  Function (..),
  Global (..),
  IndexedType,
  Kind (..),
  Scheme (..),
  TypeIndex (..),
  Uses (..),
  foldType,
  freshIdIn,
  functionExpressionRep,
  globalExpressionRep,
  normalizeRowTypes,
  typeOf,
 )
import Noll.TypeSystem (
  Assumption (..),
  Constraint (..),
  ConstraintsGenerationContext (..),
  ConstraintsGenerationError (..),
  ConstraintsGenerationOutput,
  ConstraintsGenerationStack (..),
  ConstraintsGenerationState (..),
  InferenceRule (..),
  Substitutable (..),
  Substitution (..),
  checkTypeAnnotationParameters,
  collectConstraints,
  normalizeTypeIndexes,
  runConstraintsGenerationStack,
  solveConstraints,
 )
import Noll.Utils (Dictionary, Name, (<$$>))

import qualified Data.Map.Strict as Map
import qualified Noll.Common.Environment as Environment

data CompilerEnvironment = CompilerEnvironment
  { compilerDataConstructorEnv :: Environment (Constructor TypeIndex Kind IndexedType)
  , compilerTypeConstructorEnv :: Environment Kind
  }
  deriving (Show, Eq, Ord, Read)

data CompilerState a = CompilerState
  { compilerConstraintsGenerationErrors :: [ConstraintsGenerationError a]
  , compilerTypeAnnotationParameters :: Dictionary (a, TypeIndex Kind)
  , compilerSolverRuleViolations :: [InferenceRule Kind a]
  , compilerNameEnvironment :: Environment (Scheme TypeIndex Kind IndexedType)
  , compilerSupply :: Int
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE overCompilerStateConstraintsGenerationErrors #-}
overCompilerStateConstraintsGenerationErrors :: ([ConstraintsGenerationError a] -> [ConstraintsGenerationError a]) -> CompilerState a -> CompilerState a
overCompilerStateConstraintsGenerationErrors fn CompilerState{..} = CompilerState{compilerConstraintsGenerationErrors = fn compilerConstraintsGenerationErrors, ..}

{-# INLINE overCompilerTypeAnnotationParameters #-}
overCompilerTypeAnnotationParameters :: (Dictionary (a, TypeIndex Kind) -> Dictionary (a, TypeIndex Kind)) -> CompilerState a -> CompilerState a
overCompilerTypeAnnotationParameters fn CompilerState{..} = CompilerState{compilerTypeAnnotationParameters = fn compilerTypeAnnotationParameters, ..}

{-# INLINE overCompilerSolverRuleViolations #-}
overCompilerSolverRuleViolations :: ([InferenceRule Kind a] -> [InferenceRule Kind a]) -> CompilerState a -> CompilerState a
overCompilerSolverRuleViolations fn CompilerState{..} = CompilerState{compilerSolverRuleViolations = fn compilerSolverRuleViolations, ..}

{-# INLINE overCompilerNameEnvironment #-}
overCompilerNameEnvironment :: (Environment (Scheme TypeIndex Kind IndexedType) -> Environment (Scheme TypeIndex Kind IndexedType)) -> CompilerState a -> CompilerState a
overCompilerNameEnvironment fn CompilerState{..} = CompilerState{compilerNameEnvironment = fn compilerNameEnvironment, ..}

{-# INLINE overCompilerSupply #-}
overCompilerSupply :: (Int -> Int) -> CompilerState a -> CompilerState a
overCompilerSupply fn CompilerState{..} = CompilerState{compilerSupply = fn compilerSupply, ..}

{-# INLINE initialCompilerState #-}
initialCompilerState :: CompilerState a
initialCompilerState =
  CompilerState
    { compilerConstraintsGenerationErrors = []
    , compilerTypeAnnotationParameters = mempty
    , compilerSolverRuleViolations = []
    , compilerNameEnvironment = mempty
    , compilerSupply = 0
    }

newtype Compiler a c = Compiler {compilerStack :: ReaderT CompilerEnvironment (State (CompilerState a)) c}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader CompilerEnvironment
    , MonadState (CompilerState a)
    )

{-# INLINE compilerReportConstraintsGenerationErrors #-}
compilerReportConstraintsGenerationErrors :: [ConstraintsGenerationError a] -> Compiler a ()
compilerReportConstraintsGenerationErrors errors = modify (overCompilerStateConstraintsGenerationErrors (<> errors))

{-# INLINE compilerSetTypeAnnotationParameters #-}
compilerSetTypeAnnotationParameters :: Dictionary (a, TypeIndex Kind) -> Compiler a ()
compilerSetTypeAnnotationParameters params = modify (overCompilerTypeAnnotationParameters (const params))

{-# INLINE compilerReportSolverRuleViolations #-}
compilerReportSolverRuleViolations :: [InferenceRule Kind a] -> Compiler a ()
compilerReportSolverRuleViolations errors = modify (overCompilerSolverRuleViolations (<> errors))

{-# INLINE getConstraintsGenerationErrorsC #-}
getConstraintsGenerationErrorsC :: Compiler a [ConstraintsGenerationError a]
getConstraintsGenerationErrorsC = gets compilerConstraintsGenerationErrors

{-# INLINE getSolverRuleViolationsC #-}
getSolverRuleViolationsC :: Compiler a [InferenceRule Kind a]
getSolverRuleViolationsC = gets compilerSolverRuleViolations

{-# INLINE insertNamesC #-}
insertNamesC :: [(Name, Scheme TypeIndex Kind IndexedType)] -> Compiler a ()
insertNamesC names = modify (overCompilerNameEnvironment (Environment.insertMany names))

{-# INLINE updateSupplyC #-}
updateSupplyC :: Int -> Compiler a ()
updateSupplyC supply = modify (overCompilerSupply (const supply))

{-# INLINE runCompiler #-}
runCompiler :: CompilerEnvironment -> Compiler a c -> (c, CompilerState a)
runCompiler env com = runState (runReaderT (compilerStack com) env) initialCompilerState

{-# INLINE evalCompiler #-}
evalCompiler :: CompilerEnvironment -> Compiler a c -> c
evalCompiler = fst <$$> runCompiler

type ConstraintsGenerationResult c o k t r = (r, Dictionary (c, o k), [ConstraintsGenerationOutput c o k t])

runConstraintsGenerationC :: Int -> ConstraintsGenerationStack c TypeIndex Kind IndexedType r -> Compiler a (ConstraintsGenerationResult c TypeIndex Kind IndexedType r)
runConstraintsGenerationC index stack = do
  env <- ask
  let (result, ConstraintsGenerationState{..}, output) = runConstraintsGenerationStack index (context env) stack
  updateSupplyC constraintsGenerationStateSupply
  pure (result, constraintsGenerationStateTypeIndexes, output)
 where
  context CompilerEnvironment{..} =
    ConstraintsGenerationContext
      { constraintsGenerationContextMonomorphicSet = mempty
      , constraintsGenerationContextDataConstructorEnv = compilerDataConstructorEnv
      , constraintsGenerationContextTypeConstructorEnv = compilerTypeConstructorEnv
      }

type CompilerAssumption = Assumption IndexedType

type CompilerConstraint a = Constraint (InferenceRule Kind a) TypeIndex Kind IndexedType

generateConstraintsC :: Expression a IndexedType -> Compiler a ([CompilerAssumption], [CompilerConstraint a])
generateConstraintsC e = do
  (assumptions, params, result) <- runConstraintsGenerationC (freshIdIn e) (collectConstraints e)
  let (errors, constraints) = partitionEithers result
  compilerReportConstraintsGenerationErrors errors
  compilerSetTypeAnnotationParameters params
  pure (assumptions, constraints)

assumptionConstraints :: CompilerAssumption -> Compiler a (Either CompilerAssumption (CompilerConstraint a))
assumptionConstraints Assumption{..} = do
  names <- gets compilerNameEnvironment
  case Environment.lookup assumptionName names of
    Nothing ->
      pure $ Left Assumption{..}
    Just s ->
      pure $ Right (Explicit (InferenceRule 200) assumptionType s)

solveConstraintsC :: (Show a, Eq a) => [CompilerConstraint a] -> Compiler a Substitution
solveConstraintsC cs = do
  dict <- gets compilerTypeAnnotationParameters
  let (sub, rs) = solveConstraints cs
      errors = execWriter (checkTypeAnnotationParameters (Map.toList dict) sub)
  compilerReportSolverRuleViolations (apply sub rs)
  compilerReportConstraintsGenerationErrors (IllFormedTypeAnnotation <$> errors)
  pure sub

typedExpressionC ::
  (Show a, Eq a) =>
  Expression a IndexedType ->
  Compiler a (Expression a IndexedType, [CompilerAssumption])
typedExpressionC e = do
  (as0, cs0) <- generateConstraintsC e
  (as1, cs1) <- partitionEithers <$> traverse assumptionConstraints as0
  sub <- solveConstraintsC (cs0 <> cs1)
  let e1 = normalizeRowTypes <$> apply sub e
  pure (normalizeTypeIndexes e1, apply sub as1)

typedFunctionC ::
  (Show a, Eq a) =>
  Function Expression a IndexedType ->
  Compiler a (Function Expression a IndexedType, [CompilerAssumption])
typedFunctionC f@(Function a (Uses _ t) ps e) = do
  (as0, cs0) <- generateConstraintsC (functionExpressionRep "$.internal" f)
  (as1, cs1) <- partitionEithers <$> traverse assumptionConstraints as0
  sub <- solveConstraintsC (Equality (InferenceRule 999) [t, typeOf e] : cs0 <> cs1)
  let f1 = normalizeRowTypes <$> apply sub f
  pure (normalizeTypeIndexes f1, apply sub as1)

typedGlobalC ::
  (Show a, Eq a) =>
  Global Expression a IndexedType ->
  Compiler a (Global Expression a IndexedType, [CompilerAssumption])
typedGlobalC g@(Global a (Uses _ t) e) = do
  (as0, cs0) <- generateConstraintsC (globalExpressionRep "$.internal" g)
  (as1, cs1) <- partitionEithers <$> traverse assumptionConstraints as0
  sub <- solveConstraintsC (Equality (InferenceRule 999) [t, typeOf e] : cs0 <> cs1)
  let g1 = normalizeRowTypes <$> apply sub g
  pure (normalizeTypeIndexes g1, apply sub as1)
