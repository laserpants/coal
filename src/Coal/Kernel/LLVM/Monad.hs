{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Monad stack and error types for LLVM IR code generation.

This module defines the 'IRCodegenT' monad transformer, which combines:

  * 'ReaderT' for the variable environment
  * 'ExceptT' for typed error handling
  * 'StateT' for tracking emitted type declarations and fresh-name generation
  * 'MonadIRBuilder' for LLVM IR construction primitives

The monad provides a clean abstraction for threading compiler state through
the code generation process while maintaining composability with the underlying
IR builder.
-}
module Coal.Kernel.LLVM.Monad (
  IRCodegenError (..),
  IRCodegenEnv,
  IRCodegenT (..),
  IRCodegen,
  runIRCodegenT,
  runIRCodegen,
) where

import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, MonadTrans (..), StateT, evalStateT)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import LLVM.IR

import Coal.Kernel.Language.Expr (Expr)
import Coal.Kernel.Language.Type (Type)
import Common (Name)
import Common.Environment (Environment)

data IRCodegenError
  = UnboundVariable Name
  | NotAFunctionType
  | UnsupportedExpression (Expr Type)
  | IRCycleError [Name]
  | IRMissingModules [(Name, Name)]
  deriving (Show, Eq, Ord)

type IRCodegenEnv = Environment IROperand

newtype IRCodegenT m a = IRCodegen
  { getIRCodegen :: StateT (Set Name, Int, Map ByteString Name) (ExceptT IRCodegenError (ReaderT IRCodegenEnv m)) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader IRCodegenEnv
    , MonadError IRCodegenError
    , MonadState (Set Name, Int, Map ByteString Name)
    , MonadIRBuilder
    , MonadFix
    )

type IRCodegen = IRCodegenT IRBuilder

runIRCodegenT :: (Monad m) => IRCodegenEnv -> IRCodegenT m a -> m (Either IRCodegenError a)
runIRCodegenT env comp = runReaderT (runExceptT (evalStateT (getIRCodegen comp) (Set.empty, 0, Map.empty))) env

runIRCodegen :: IRCodegenEnv -> IRCodegen a -> IRBuilder (Either IRCodegenError a)
runIRCodegen = runIRCodegenT

instance MonadTrans IRCodegenT where
  lift = IRCodegen . lift . lift . lift
