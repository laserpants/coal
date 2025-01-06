{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler (
  CompilerEnvironment (..),
  Compiler (..),
  runCompiler,
  indexedExpression,
  runAggregationStackInCompiler,
) where

import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (MonadState, State, evalState, runState)
import Noll.Language (
  Constructor (..),
  Expression (..),
  IndexedType,
  Kind (..),
  Type (..),
  TypeIndex (..),
 )
import Noll.Library.Environment (Environment (..))
import Noll.Library.Supply (supply)
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
runCompiler env comp = runState (runReaderT (compilerStack comp) env) initialCompilerState

indexedExpression :: Expression a () -> Compiler (Expression a (Type TypeIndex Kind))
indexedExpression expr = pure (evalState (traverse (fmap tVar . const supply) expr) 0)
 where
  tVar = TVariable . TypeIndex KType

type AggregationStackResult a o k t c = (c, Dictionary (a, o k), [AggregationOutput a o k t])

runAggregationStackInCompiler ::
  Int ->
  AggregationStack a TypeIndex Kind IndexedType c ->
  Compiler (AggregationStackResult a TypeIndex Kind IndexedType c)
runAggregationStackInCompiler index stack = do
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
