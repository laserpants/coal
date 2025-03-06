{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.LLVM.IRInstruction.Interpreter.State (
  IRInterpreterState (..),
  IRInterpreterArtifact (..),
  addArtifact,
  setLabel,
  incrementLabelIndex,
  incrementRegisterIndex,
  initialIRInterpreterState,
) where

import Control.Monad.RWS (MonadState, modify)
import Data.Text (Text)
import Noll.Utils (Name, Over)

data IRInterpreterArtifact
  = InterpreterArtifactFunctionApply Int
  | InterpreterArtifactClosure Int
  | InterpreterArtifactHashMapKey Name
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
    , irInterpreterStateLabel = ""
    , irInterpreterStateArtifacts = []
    }

{-# INLINE overIRInterpreterStateRegisterIndex #-}
overIRInterpreterStateRegisterIndex :: Over IRInterpreterState Int
overIRInterpreterStateRegisterIndex f IRInterpreterState{..} = IRInterpreterState{irInterpreterStateRegisterIndex = f irInterpreterStateRegisterIndex, ..}

{-# INLINE overIRInterpreterStateLabelIndex #-}
overIRInterpreterStateLabelIndex :: Over IRInterpreterState Int
overIRInterpreterStateLabelIndex f IRInterpreterState{..} = IRInterpreterState{irInterpreterStateLabelIndex = f irInterpreterStateLabelIndex, ..}

{-# INLINE overIRInterpreterStateLabel #-}
overIRInterpreterStateLabel :: Over IRInterpreterState Text
overIRInterpreterStateLabel f IRInterpreterState{..} = IRInterpreterState{irInterpreterStateLabel = f irInterpreterStateLabel, ..}

{-# INLINE overIRInterpreterStateArtifacts #-}
overIRInterpreterStateArtifacts :: Over IRInterpreterState [IRInterpreterArtifact]
overIRInterpreterStateArtifacts f IRInterpreterState{..} = IRInterpreterState{irInterpreterStateArtifacts = f irInterpreterStateArtifacts, ..}

{-# INLINE incrementRegisterIndex #-}
incrementRegisterIndex :: (MonadState IRInterpreterState m) => m ()
incrementRegisterIndex = modify (overIRInterpreterStateRegisterIndex succ)

{-# INLINE incrementLabelIndex #-}
incrementLabelIndex :: (MonadState IRInterpreterState m) => m ()
incrementLabelIndex = modify (overIRInterpreterStateLabelIndex succ)

{-# INLINE setLabel #-}
setLabel :: (MonadState IRInterpreterState m) => Text -> m ()
setLabel label = modify (overIRInterpreterStateLabel (const label))

{-# INLINE addArtifact #-}
addArtifact :: (MonadState IRInterpreterState m) => IRInterpreterArtifact -> m ()
addArtifact art = modify (overIRInterpreterStateArtifacts (art :))
