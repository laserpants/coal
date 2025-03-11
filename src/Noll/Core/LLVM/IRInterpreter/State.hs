{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.LLVM.IRInterpreter.State (
  IRInterpreterState (..),
  initialIRInterpreterState,
  incrementRegisterIndex,
  incrementLabelIndex,
  addArtifact,
  setLabel,
) where

import Control.Monad.RWS (MonadState, modify)
import Data.Text (Text)
import Noll.Utils (Over)

data IRInterpreterState a = IRInterpreterState
  { irInterpreterStateRegisterIndex :: Int
  , irInterpreterStateLabelIndex :: Int
  , irInterpreterStateLabel :: Text
  , irInterpreterStateArtifacts :: [a]
  }
  deriving (Show, Eq, Ord)

{-# INLINE initialIRInterpreterState #-}
initialIRInterpreterState :: IRInterpreterState a
initialIRInterpreterState =
  IRInterpreterState
    { irInterpreterStateRegisterIndex = 1
    , irInterpreterStateLabelIndex = 1
    , irInterpreterStateLabel = ""
    , irInterpreterStateArtifacts = []
    }

{-# INLINE overIRInterpreterStateRegisterIndex #-}
overIRInterpreterStateRegisterIndex :: Over (IRInterpreterState a) Int
overIRInterpreterStateRegisterIndex f IRInterpreterState{..} = IRInterpreterState{irInterpreterStateRegisterIndex = f irInterpreterStateRegisterIndex, ..}

{-# INLINE overIRInterpreterStateLabelIndex #-}
overIRInterpreterStateLabelIndex :: Over (IRInterpreterState a) Int
overIRInterpreterStateLabelIndex f IRInterpreterState{..} = IRInterpreterState{irInterpreterStateLabelIndex = f irInterpreterStateLabelIndex, ..}

{-# INLINE overIRInterpreterStateLabel #-}
overIRInterpreterStateLabel :: Over (IRInterpreterState a) Text
overIRInterpreterStateLabel f IRInterpreterState{..} = IRInterpreterState{irInterpreterStateLabel = f irInterpreterStateLabel, ..}

{-# INLINE overIRInterpreterStateArtifacts #-}
overIRInterpreterStateArtifacts :: Over (IRInterpreterState a) [a]
overIRInterpreterStateArtifacts f IRInterpreterState{..} = IRInterpreterState{irInterpreterStateArtifacts = f irInterpreterStateArtifacts, ..}

{-# INLINE incrementRegisterIndex #-}
incrementRegisterIndex :: (MonadState (IRInterpreterState a) m) => m ()
incrementRegisterIndex = modify (overIRInterpreterStateRegisterIndex succ)

{-# INLINE incrementLabelIndex #-}
incrementLabelIndex :: (MonadState (IRInterpreterState a) m) => m ()
incrementLabelIndex = modify (overIRInterpreterStateLabelIndex succ)

{-# INLINE setLabel #-}
setLabel :: (MonadState (IRInterpreterState a) m) => Text -> m ()
setLabel label = modify (overIRInterpreterStateLabel (const label))

{-# INLINE addArtifact #-}
addArtifact :: (MonadState (IRInterpreterState a) m) => a -> m ()
addArtifact art = modify (overIRInterpreterStateArtifacts (art :))
