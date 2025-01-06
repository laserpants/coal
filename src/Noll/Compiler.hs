{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler (
  CompilerEnvironment (..),
  Compiler (..),
  runCompiler,
  tagExpression,
 ) where

import Noll.Library.Supply (supply)
import Control.Monad.State (MonadState, State, evalState, runState)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Noll.Language (
  Expression (..),
  Type (..),
  TypeIndex (..),
  Kind (..),
 )

data CompilerEnvironment = CompilerEnvironment

data CompilerState = CompilerState

newtype Compiler a = Compiler { compilerStack :: ReaderT CompilerEnvironment (State CompilerState) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader CompilerEnvironment
    , MonadState CompilerState
    )

runCompiler :: CompilerEnvironment -> Compiler a -> (a, CompilerState)
runCompiler env c = runState (runReaderT (compilerStack c) env) CompilerState{}

tagExpression :: Expression a () -> Compiler (Expression a (Type TypeIndex Kind))
tagExpression expr = pure (fmap tVar (evalState (traverse (const supply) expr) 0))
  where
    tVar = TVariable . TypeIndex KType
