{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Noll.Core.LLVM.IRInstruction.Interpreter (
  interpret,
  evalInterpreter,
  runInterpreter,
) where

import Control.Monad.Free (iterM)
import Control.Monad.RWS (MonadReader, MonadState, MonadWriter, RWS, runRWS)
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
initialIRInterpreterState =
  IRInterpreterState
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

data IRLine
  = LInstruction [Text]
  | LComment Text
  | LLabel Text
  deriving (Show, Eq, Ord)

newtype IRInterpreter a = IRInterpreter {getIRInterpreter :: RWS IRInterpreterEnv [IRLine] IRInterpreterState a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState IRInterpreterState
    , MonadWriter [IRLine]
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

evalInterpreter :: IRInterpreterEnv -> IRInterpreter a -> [IRLine]
evalInterpreter env ipt = undefined -- evalState (execWriterT (runReaderT (getIRInterpreter ipt) env)) initialIRInterpreterState

runInterpreter :: IRInterpreterEnv -> IRInterpreter a -> (a, IRInterpreterState, [IRLine])
runInterpreter env ipt = runRWS (getIRInterpreter ipt) undefined undefined
