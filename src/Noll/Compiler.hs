{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler (
  CompilerEnvironment (..),
  Compiler (..),
  runCompiler,
  runConstraintsGenerationC,
) where

import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (MonadState, State, modify, runState)
import Data.Either.Extra (partitionEithers)
import Noll.Language (
  Constructor (..),
  Expression (..),
  IndexedType,
  Kind (..),
  Type (..),
  TypeIndex (..),
  freshIdIn,
 )
import Noll.Library.Environment (Environment (..))
import Noll.TypeSystem.Constraint (Constraint (..))
import Noll.TypeSystem.Constraint.Aggregation (collectConstraints)
import Noll.TypeSystem.Constraint.Aggregation.Internal (
  AggregationContext (..),
  AggregationOutput (..),
  AggregationStack (..),
  ConstraintsGenerationError (..),
  InferenceRule (..),
  runAggregationStack,
 )
import Noll.TypeSystem.Constraint.Assumption (Assumption (..))
import Noll.TypeSystem.Constraint.Solver (Solver (..))
import Noll.TypeSystem.Substitution (Substitution (..))
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
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE overCompilerStateConstraintsGenerationErrors #-}
overCompilerStateConstraintsGenerationErrors :: ([ConstraintsGenerationError a] -> [ConstraintsGenerationError a]) -> CompilerState a -> CompilerState a
overCompilerStateConstraintsGenerationErrors fn CompilerState{..} = CompilerState{compilerConstraintsGenerationErrors = fn compilerConstraintsGenerationErrors, ..}

{-# INLINE overCompilerTypeAnnotationParameters #-}
overCompilerTypeAnnotationParameters :: (Dictionary (a, TypeIndex Kind) -> Dictionary (a, TypeIndex Kind)) -> CompilerState a -> CompilerState a
overCompilerTypeAnnotationParameters fn CompilerState{..} = CompilerState{compilerTypeAnnotationParameters = fn compilerTypeAnnotationParameters, ..}

{-# INLINE initialCompilerState #-}
initialCompilerState :: CompilerState a
initialCompilerState =
  CompilerState
    { compilerConstraintsGenerationErrors = []
    , compilerTypeAnnotationParameters = mempty
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

{-# INLINE runCompiler #-}
runCompiler :: CompilerEnvironment -> Compiler a c -> (c, CompilerState a)
runCompiler env com = runState (runReaderT (compilerStack com) env) initialCompilerState

{-# INLINE evalCompiler #-}
evalCompiler :: CompilerEnvironment -> Compiler a c -> c
evalCompiler = fst <$$> runCompiler

type ConstraintsGenerationResult c o k t r = (r, Dictionary (c, o k), [AggregationOutput c o k t])

runConstraintsGenerationC :: Int -> AggregationStack c TypeIndex Kind IndexedType r -> Compiler a (ConstraintsGenerationResult c TypeIndex Kind IndexedType r)
runConstraintsGenerationC index stack = do
  env <- ask
  pure (runAggregationStack (context env) stack)
 where
  context CompilerEnvironment{..} =
    AggregationContext
      { aggregationMonomorphicSet = mempty
      , aggregationDataConstructorEnv = compilerDataConstructorEnv
      , aggregationTypeConstructorEnv = compilerTypeConstructorEnv
      , aggregationIndexTreshold = index
      }

generateConstraintsC :: Expression a IndexedType -> Compiler a ([Assumption IndexedType], [Constraint (InferenceRule Kind a) TypeIndex Kind IndexedType])
generateConstraintsC e = do
  (assumptions, params, result) <- runConstraintsGenerationC (freshIdIn e) (collectConstraints e)
  let (errors, constraints) = partitionEithers result
  compilerReportConstraintsGenerationErrors errors
  compilerSetTypeAnnotationParameters params
  pure (assumptions, constraints)

runSolverC :: Solver c o k t -> Compiler a r
runSolverC =
  undefined

solveConstraintsC :: [Constraint (InferenceRule Kind a) TypeIndex Kind IndexedType] -> Compiler a Substitution
solveConstraintsC constraints = undefined
