{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Solver (Solver (..), SolverError (..), runSolver, evalSolver) where

import Control.Monad.Writer (MonadWriter, WriterT, runWriterT)
import Control.Monad.State (MonadState, State, runState)

data SolverError = SolverError
  deriving (Show, Eq, Ord, Read)

newtype Solver a = Solver {solverMonad :: WriterT [SolverError] (State Int) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState Int
    , MonadWriter [SolverError]
    )

{-# INLINE runSolver #-}
runSolver :: Int -> Solver a -> ((a, [SolverError]), Int)
runSolver n u = runState (runWriterT (solverMonad u)) n

{-# INLINE evalSolver #-}
evalSolver :: Int -> Solver a -> (a, [SolverError])
evalSolver n u = fst (runSolver n u)
