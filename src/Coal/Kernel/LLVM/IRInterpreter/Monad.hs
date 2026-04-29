{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Kernel.LLVM.IRInterpreter.Monad (
  IRLine (..),
  IRInterpreter (..),
  evalInterpreter,
  runInterpreter,
  nextLabelIndex,
  nextRegister,
  refreshInterpreterState,
  throwIRError,
) where

import Coal.Kernel.LLVM.IREncodable (IREncodable (..), enquote)
import Coal.Kernel.LLVM.IRError (IRGenError)
import Coal.Kernel.LLVM.IRInterpreter.Artifact (IRInterpreterArtifact)
import Coal.Kernel.LLVM.IRInterpreter.Environment (IRInterpreterEnv)
import Coal.Kernel.LLVM.IRInterpreter.State
import Coal.Kernel.LLVM.IRType (IRType (..))
import Coal.Kernel.LLVM.IRValue (IRValue (..))
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.RWS (MonadReader, MonadState, MonadWriter, RWS, evalRWS, gets, modify, runRWS)
import Data.Text (Text)
import qualified Data.Text as Text
import TextShow (showt)

data IRLine
  = LInstruction [Text]
  | LComment Text
  | LLabel Text
  deriving (Show, Eq, Ord)

instance IREncodable IRLine where
  irEncode =
    \case
      LInstruction tokens ->
        "  " <> Text.unwords tokens
      LComment text ->
        "  ; " <> text
      LLabel text ->
        enquote text <> ":"

type IRInterpreterStack a = ExceptT IRGenError (RWS IRInterpreterEnv [IRLine] (IRInterpreterState IRInterpreterArtifact)) a

newtype IRInterpreter a = IRInterpreter {getIRInterpreter :: IRInterpreterStack a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState (IRInterpreterState IRInterpreterArtifact)
    , MonadWriter [IRLine]
    , MonadReader IRInterpreterEnv
    , MonadError IRGenError
    )

evalInterpreter :: IRInterpreterEnv -> IRInterpreter a -> (Either IRGenError a, [IRLine])
evalInterpreter env ir =
  let (result, logs) = evalRWS (runExceptT (getIRInterpreter ir)) env initialIRInterpreterState
   in (result, logs)

runInterpreter :: IRInterpreterEnv -> IRInterpreter a -> (Either IRGenError a, IRInterpreterState IRInterpreterArtifact, [IRLine])
runInterpreter env ir =
  let (result, state, logs) = runRWS (runExceptT (getIRInterpreter ir)) env initialIRInterpreterState
   in (result, state, logs)

refreshInterpreterState :: IRInterpreter ()
refreshInterpreterState = modify resetIRInterpreterState

nextLabelIndex :: IRInterpreter Int
nextLabelIndex = do
  incrementLabelIndex
  gets irInterpreterStateLabelIndex

throwIRError :: IRGenError -> IRInterpreter a
throwIRError = throwError
nextRegister :: IRType -> IRInterpreter IRValue
nextRegister t = do
  n <- gets irInterpreterStateRegisterIndex
  incrementRegisterIndex
  pure (Local t (showt n))
