{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.LLVM.IRInterpreter.Monad (
  IRLine (..),
  IRInterpreter (..),
  evalInterpreter,
  runInterpreter,
  nextLabelIndex,
  nextRegister,
) where

import Control.Monad.RWS (MonadReader, MonadState, MonadWriter, RWS, evalRWS, gets, runRWS)
import Data.Text (Text)
import Noll.Core.LLVM.IREncodable (IREncodable (..), enquote)
import Noll.Core.LLVM.IRInterpreter.Artifact
import Noll.Core.LLVM.IRInterpreter.Environment
import Noll.Core.LLVM.IRInterpreter.State
import Noll.Core.LLVM.IRType (IRType (..))
import Noll.Core.LLVM.IRValue (IRValue (..))
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

newtype IRInterpreter a = IRInterpreter {getIRInterpreter :: RWS IRInterpreterEnv [IRLine] (IRInterpreterState IRInterpreterArtifact) a}
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

nextLabelIndex :: IRInterpreter Int
nextLabelIndex = do
  incrementLabelIndex
  gets irInterpreterStateLabelIndex

nextRegister :: IRType -> IRInterpreter IRValue
nextRegister t = do
  n <- gets irInterpreterStateRegisterIndex
  incrementRegisterIndex
  pure (Local t (showt n))
