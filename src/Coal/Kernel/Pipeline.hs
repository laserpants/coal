{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Normalization pipeline monad and state.

The pipeline transformer provides:

  * Fresh name generation for creating unique identifiers during
    transformations
  * Structured error handling for transformation failures
  * Composability via the 'Pass' type synonym

Normalization passes transform modules through a series of rewrites, each
establishing stronger invariants about the program structure. The pipeline is
designed to be run sequentially, with each pass depending on the invariants
established by prior passes.
-}
module Coal.Kernel.Pipeline (
  -- * Pipeline monad
  PipelineT (..),
  Pipeline,
  Pass,

  -- * State and errors
  PipelineState (..),
  PipelineError (..),

  -- * Running
  initialPipelineState,
  runPipelineT,
  evalPipelineT,
  runPipeline,
  evalPipeline,

  -- * Internals
  overPipelineFreshCounter,
  getFreshCounter,
  freshName,
) where

import Common (Name)
import Control.Monad.Except (
  ExceptT,
  MonadError,
  MonadIO,
  MonadTrans (..),
  runExceptT,
 )
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (MonadState, StateT, gets, modify, runStateT)
import qualified Data.Text as Text

newtype PipelineState = PipelineState
  { pipelineFreshCounter :: Int
  }
  deriving (Show, Eq, Ord)

data PipelineError
  = -- | A constructor was applied to more arguments than its declared arity.
    OverSaturatedConstructor Name
  deriving (Show, Eq, Ord)

{- | Pipeline transformer: 'StateT' over 'ExceptT', giving mutable fresh-name
state and structured error handling in any base monad @m@.
-}
newtype PipelineT m a = PipelineT {unPipelineT :: StateT PipelineState (ExceptT PipelineError m) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState PipelineState
    , MonadError PipelineError
    , MonadIO
    )

-- | Specialization of 'PipelineT' to the pure 'Identity' base monad.
type Pipeline = PipelineT Identity

instance MonadTrans PipelineT where
  lift = PipelineT . lift . lift

{- | A single compilation pass: transforms a value of type @i@ into @o@,
potentially advancing the fresh-name counter or signalling an error.
-}
type Pass m i o = i -> PipelineT m o

-- ---------------------------------------------------------------------------
-- State helpers
-- ---------------------------------------------------------------------------

-- | Initial pipeline state with the fresh-name counter set to zero.
initialPipelineState :: PipelineState
initialPipelineState = PipelineState{pipelineFreshCounter = 0}

overPipelineFreshCounter :: (Int -> Int) -> PipelineState -> PipelineState
overPipelineFreshCounter fn PipelineState{..} =
  PipelineState{pipelineFreshCounter = fn pipelineFreshCounter}

getFreshCounter :: (Monad m) => PipelineT m Int
getFreshCounter = do
  n <- gets pipelineFreshCounter
  modify (overPipelineFreshCounter (+ 1))
  return n

{- | Allocate a fresh name of the form @$$\<prefix\>.\<n\>@, where @n@ is a
globally-unique counter within the current pipeline run.
-}
freshName :: (Monad m) => Name -> PipelineT m Name
freshName prefix = do
  n <- getFreshCounter
  return $ prefix <> "." <> Text.pack (show n)

-- ---------------------------------------------------------------------------
-- Runners
-- ---------------------------------------------------------------------------

{- | Run a 'PipelineT' action from the given initial state, returning the
result together with the final state, or the first 'PipelineError'.
-}
runPipelineT ::
  PipelineState ->
  PipelineT m a ->
  m (Either PipelineError (a, PipelineState))
runPipelineT s p = runExceptT (runStateT (unPipelineT p) s)

-- | Like 'runPipelineT', but discards the final 'PipelineState' on success.
evalPipelineT ::
  (Monad m) =>
  PipelineState ->
  PipelineT m a ->
  m (Either PipelineError a)
evalPipelineT s = fmap (fmap fst) . runPipelineT s

-- | Run a pure 'Pipeline' action, returning the result and final state.
runPipeline :: PipelineState -> Pipeline a -> Either PipelineError (a, PipelineState)
runPipeline s = runIdentity . runPipelineT s

-- | Run a pure 'Pipeline' action, discarding the final state.
evalPipeline :: PipelineState -> Pipeline a -> Either PipelineError a
evalPipeline s = runIdentity . evalPipelineT s
