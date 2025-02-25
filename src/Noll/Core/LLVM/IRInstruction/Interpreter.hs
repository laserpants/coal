{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Noll.Core.LLVM.IRInstruction.Interpreter (interpret) where

import Control.Monad.Free (iterM)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, State, evalState, get, gets, modify, runState)
import Control.Monad.Writer (MonadWriter, WriterT, execWriterT, tell)
import Data.Text (Text)
import Noll.Common.Environment (Environment (..))
import Noll.Core.LLVM.IRInstruction (IRInstr, IRInstrOp, IRInstrOpF (..))
import Noll.Core.LLVM.IRValue (IRValue (..))

data IRInterpreterArtifact = IRInterpreterArtifact
  deriving (Show, Eq, Ord)

data IRInterpreterState = IRInterpreterState
  { irInterpreterStateRegisterIndex :: Int
  , irInterpreterStateLabelIndex :: Int
  , irInterpreterStateLabel :: Text
  , irInterpreterStateArtifacts :: [IRInterpreterArtifact]
  }
  deriving (Show, Eq, Ord)

{-# INLINE initialIRInterpreterState #-}
initialIRInterpreterState :: IRInterpreterState
initialIRInterpreterState = IRInterpreterState
  { irInterpreterStateRegisterIndex = 1
  , irInterpreterStateLabelIndex = 1
  , irInterpreterStateLabel = mempty
  , irInterpreterStateArtifacts = []
  }

data IRInterpreterEnv = IRInterpreterEnv
  { irInterpreterValueEnv :: Environment IRValue
  , irInterpreterConstructorEnv :: Environment Int
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

{-# INLINE interpret #-}
interpret :: IRInstr a -> IRInterpreter a
interpret = iterM interpreter

evalInterpreter :: IRInterpreterEnv -> IRInterpreter a -> [Line]
evalInterpreter env ipt = evalState (execWriterT (runReaderT (getIRInterpreter ipt) env)) initialIRInterpreterState

runInterpreter :: IRInterpreterEnv -> IRInterpreter a -> ([Line], IRInterpreterState)
runInterpreter env ipt = runState (execWriterT (runReaderT (getIRInterpreter ipt) env)) initialIRInterpreterState
