{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.LLVM.IRInstruction.Interpreter.Types (
  IRLine (..),
  IRInterpreter (..),
  nextLabelIndex,
  nextRegister,
  evalInterpreter,
  runInterpreter,
) where

import Control.Monad.RWS (
  MonadReader,
  MonadState,
  MonadWriter,
  RWS,
  evalRWS,
  gets,
  runRWS,
 )
import Data.Text (Text)
import Noll.Core.LLVM.IREncodable (IREncodable (..), enquote)
import Noll.Core.LLVM.IRInstruction.Interpreter.Environment (IRInterpreterEnv (..))
import Noll.Core.LLVM.IRInstruction.Interpreter.State (
  IRInterpreterState (..),
  incrementLabelIndex,
  incrementRegisterIndex,
  initialIRInterpreterState,
 )
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

newtype IRInterpreter a = IRInterpreter {getIRInterpreter :: RWS IRInterpreterEnv [IRLine] IRInterpreterState a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState IRInterpreterState
    , MonadWriter [IRLine]
    , MonadReader IRInterpreterEnv
    )

evalInterpreter :: IRInterpreterEnv -> IRInterpreter a -> (a, [IRLine])
evalInterpreter env ri = evalRWS (getIRInterpreter ri) env initialIRInterpreterState

runInterpreter :: IRInterpreterEnv -> IRInterpreter a -> (a, IRInterpreterState, [IRLine])
runInterpreter env ri = runRWS (getIRInterpreter ri) env initialIRInterpreterState

nextLabelIndex :: IRInterpreter Int
nextLabelIndex = do
  incrementLabelIndex
  gets irInterpreterStateLabelIndex

nextRegister :: IRType -> IRInterpreter IRValue
nextRegister t = do
  n <- gets irInterpreterStateRegisterIndex
  incrementRegisterIndex
  pure (Local t (showt n))
