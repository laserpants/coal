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
) where

import Coal.Kernel.LLVM.IREncodable (IREncodable (..), enquote)
import Coal.Kernel.LLVM.IRInterpreter.Artifact
import Coal.Kernel.LLVM.IRInterpreter.Environment
import Coal.Kernel.LLVM.IRInterpreter.State
import Coal.Kernel.LLVM.IRType (IRType (..))
import Coal.Kernel.LLVM.IRValue (IRValue (..))
import Control.Monad.RWS (MonadReader, MonadState, MonadWriter, RWS, evalRWS, gets, modify, runRWS)
import Data.Text (Text)
import TextShow (showt)

import qualified Data.Text as Text

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

type IRInterpreterStack a = RWS IRInterpreterEnv [IRLine] (IRInterpreterState IRInterpreterArtifact) a

newtype IRInterpreter a = IRInterpreter {getIRInterpreter :: IRInterpreterStack a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState (IRInterpreterState IRInterpreterArtifact)
    , MonadWriter [IRLine]
    , MonadReader IRInterpreterEnv
    )

evalInterpreter :: IRInterpreterEnv -> IRInterpreter a -> (a, [IRLine])
evalInterpreter env ir = evalRWS (getIRInterpreter ir) env initialIRInterpreterState

runInterpreter :: IRInterpreterEnv -> IRInterpreter a -> (a, IRInterpreterState IRInterpreterArtifact, [IRLine])
runInterpreter env ir = runRWS (getIRInterpreter ir) env initialIRInterpreterState

refreshInterpreterState :: IRInterpreter ()
refreshInterpreterState = modify resetIRInterpreterState

nextLabelIndex :: IRInterpreter Int
nextLabelIndex = do
  incrementLabelIndex
  gets irInterpreterStateLabelIndex

nextRegister :: IRType -> IRInterpreter IRValue
nextRegister t = do
  n <- gets irInterpreterStateRegisterIndex
  incrementRegisterIndex
  pure (Local t (showt n))
