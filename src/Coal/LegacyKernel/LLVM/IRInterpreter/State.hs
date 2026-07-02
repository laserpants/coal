{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.LegacyKernel.LLVM.IRInterpreter.State (
  IRInterpreterState (..),
  initialIRInterpreterState,
  incrementRegisterIndex,
  incrementLabelIndex,
  resetIRInterpreterState,
  addArtifact,
  setLabel,
  setCurrentFunction,
  getCurrentFunction,
) where

import Control.Monad.RWS (MonadState, modify)
import Control.Monad.State (gets)
import Data.Text (Text)
import Extras (Name, Over)

data IRInterpreterState a = IRInterpreterState
  { irInterpreterStateRegisterIndex :: Int
  , irInterpreterStateLabelIndex :: Int
  , irInterpreterStateLabel :: Text
  , irInterpreterStateArtifacts :: [a]
  , irInterpreterStateCurrentFunction :: Maybe Name
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
    , irInterpreterStateCurrentFunction = Nothing
    }

resetIRInterpreterState :: IRInterpreterState a -> IRInterpreterState a
resetIRInterpreterState IRInterpreterState{..} =
  IRInterpreterState
    { irInterpreterStateRegisterIndex = 1
    , irInterpreterStateLabel = ""
    , ..
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

{-# INLINE setCurrentFunction #-}
setCurrentFunction :: (MonadState (IRInterpreterState a) m) => Maybe Name -> m ()
setCurrentFunction name = modify (\s -> s{irInterpreterStateCurrentFunction = name})

{-# INLINE getCurrentFunction #-}
getCurrentFunction :: (MonadState (IRInterpreterState a) m) => m (Maybe Name)
getCurrentFunction = gets irInterpreterStateCurrentFunction
