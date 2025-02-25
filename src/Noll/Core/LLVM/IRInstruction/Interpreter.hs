{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Noll.Core.LLVM.IRInstruction.Interpreter where

import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.State (MonadState, State, evalState, get, gets, modify, runState)
import Control.Monad.Writer (MonadWriter, WriterT, execWriterT, tell)
import Data.Text (Text)
import Noll.Common.Environment (Environment (..))
import Noll.Core.LLVM.IRInstruction (IRInstrOp, IRInstrOpF (..))
import Noll.Core.LLVM.IRValue (IRValue (..))

data IRInterpreterState = IRInterpreterState

data IRInterpreterEnv = IRInterpreterEnv
  { irCodeValueEnv :: Environment IRValue
  , irCodeConstructorEnv :: Environment Int
  }
  deriving (Show, Eq, Ord, Read)

data Line
  = LInstruction [Text]
  | LComment Text
  | LLabel Text
  deriving (Show, Eq, Ord)

-- TODO: Use RWS
newtype IRInterpreter a = IRInterpreter {getIRInterpreter :: ReaderT IRInterpreterEnv (WriterT [Line] (State IRInterpreterState)) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState IRInterpreterState
    , MonadWriter [Line]
    , MonadReader IRInterpreterEnv
    )

interpreter :: IRInstrOp (IRInterpreter a) -> IRInterpreter a
interpreter =
  \case
    IAdd t v1 v2 next ->
      undefined
