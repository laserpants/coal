{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler (
  CompilerEnvironment (..),
  Compiler (..),
  runCompiler,
  runConstraintsGenerationInCompiler,
) where

import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (MonadState, State, runState)
import Noll.Language (
  Constructor (..),
  Expression (..),
  IndexedType,
  Kind (..),
  Type (..),
  TypeIndex (..),
 )
import Noll.Library.Environment (Environment (..))
import Noll.TypeSystem.Constraint.Aggregation.Internal (AggregationContext (..), AggregationOutput (..), AggregationStack (..), runAggregationStack)
import Noll.Utils (Dictionary)

data CompilerError
  = Error1
  deriving (Show, Eq, Ord, Read)

data CompilerEnvironment = CompilerEnvironment
  { compilerDataConstructorEnv :: Environment (Constructor TypeIndex Kind IndexedType)
  , compilerTypeConstructorEnv :: Environment Kind
  }
  deriving (Show, Eq, Ord, Read)

data CompilerState = CompilerState
  { compilerErrors :: [CompilerError]
  }
  deriving (Show, Eq, Ord, Read)

initialCompilerState :: CompilerState
initialCompilerState =
  CompilerState
    { compilerErrors = []
    }

newtype Compiler a = Compiler {compilerStack :: ReaderT CompilerEnvironment (State CompilerState) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader CompilerEnvironment
    , MonadState CompilerState
    )

runCompiler :: CompilerEnvironment -> Compiler a -> (a, CompilerState)
runCompiler env com = runState (runReaderT (compilerStack com) env) initialCompilerState

type ConstraintsGenerationResult c o k t r = (r, Dictionary (c, o k), [AggregationOutput c o k t])

runConstraintsGenerationInCompiler ::
  Int ->
  AggregationStack c TypeIndex Kind IndexedType r ->
  Compiler (ConstraintsGenerationResult c TypeIndex Kind IndexedType r)
runConstraintsGenerationInCompiler index stack = do
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
