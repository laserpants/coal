{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler (
  CompilerEnvironment (..),
  Compiler (..),
  CompilerState (..),
  runCompiler,
  evalCompiler,
  runConstraintsGenerationC,
  generateConstraintsC,
  solveConstraintsC,
  getConstraintsGenerationErrorsC,
  getSolverRuleViolationsC,
) where

import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (MonadState, State, gets, modify, runState)
import Control.Monad.Writer (execWriter)
import Data.Either.Extra (partitionEithers)
import Noll.Language (
  Constructor (..),
  Expression (..),
  IndexedType,
  Kind (..),
  TypeIndex (..),
  freshIdIn,
 )
import Noll.Lib.Environment (Environment (..))
import Noll.TypeSystem (
  Assumption (..),
  Constraint (..),
  ConstraintsGenerationContext (..),
  ConstraintsGenerationError (..),
  ConstraintsGenerationOutput,
  ConstraintsGenerationStack (..),
  InferenceRule (..),
  Substitutable (..),
  Substitution (..),
  checkTypeAnnotationParameters,
  collectConstraints,
  runConstraintsGenerationStack,
  solveConstraints,
 )
import Noll.Utils (Dictionary, (<$$>))

import qualified Data.Map.Strict as Map

data CompilerEnvironment = CompilerEnvironment
  { compilerDataConstructorEnv :: Environment (Constructor TypeIndex Kind IndexedType)
  , compilerTypeConstructorEnv :: Environment Kind
  }
  deriving (Show, Eq, Ord, Read)

data CompilerState a = CompilerState
  { compilerConstraintsGenerationErrors :: [ConstraintsGenerationError a]
  , compilerTypeAnnotationParameters :: Dictionary (a, TypeIndex Kind)
  , compilerSolverRuleViolations :: [InferenceRule Kind a]
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

{-# INLINE initialCompilerState #-}
initialCompilerState :: CompilerState a
initialCompilerState =
  CompilerState
    { compilerConstraintsGenerationErrors = []
    , compilerTypeAnnotationParameters = mempty
    , compilerSolverRuleViolations = []
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

getConstraintsGenerationErrorsC :: Compiler a [ConstraintsGenerationError a]
getConstraintsGenerationErrorsC = gets compilerConstraintsGenerationErrors

getSolverRuleViolationsC :: Compiler a [InferenceRule Kind a]
getSolverRuleViolationsC = gets compilerSolverRuleViolations

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
  pure (runConstraintsGenerationStack (context env) stack)
 where
  context CompilerEnvironment{..} =
    ConstraintsGenerationContext
      { constraintsGenerationMonomorphicSet = mempty
      , constraintsGenerationDataConstructorEnv = compilerDataConstructorEnv
      , constraintsGenerationTypeConstructorEnv = compilerTypeConstructorEnv
      , constraintsGenerationIndexTreshold = index
      }

generateConstraintsC :: Expression a IndexedType -> Compiler a ([Assumption IndexedType], [Constraint (InferenceRule Kind a) TypeIndex Kind IndexedType])
generateConstraintsC e = do
  (assumptions, params, result) <- runConstraintsGenerationC (freshIdIn e) (collectConstraints e)
  let (errors, constraints) = partitionEithers result
  compilerReportConstraintsGenerationErrors errors
  compilerSetTypeAnnotationParameters params
  pure (assumptions, constraints)

solveConstraintsC :: (Eq a) => [Constraint (InferenceRule Kind a) TypeIndex Kind IndexedType] -> Compiler a Substitution
solveConstraintsC cs = do
  dict <- gets compilerTypeAnnotationParameters
  let (sub, rs) = solveConstraints cs
      errors = execWriter (checkTypeAnnotationParameters (Map.toList dict) sub)
  compilerReportSolverRuleViolations (apply sub rs)
  compilerReportConstraintsGenerationErrors (IllFormedTypeAnnotation <$> errors)
  pure sub
