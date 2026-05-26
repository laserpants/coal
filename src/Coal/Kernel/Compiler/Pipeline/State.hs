{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Kernel.Compiler.Pipeline.State (
  PipelineState (..),
  initialPipelineState,
  resetPipelineState,
  overPipelineStateSupply,
  overPipelineStateInterpreterEnv,
  overPipelineStateInterpreterValueEnv,
  overPipelineStateInterpreterConstructorEnv,
  overPipelineStateInterpreterIRTypes,
  overPipelineStateArtifacts,
  overPipelineStateCode,
  overPipelineStateNames,
  overPipelineStateIRTypes,
  overPipelineStateConstructors,
) where

import Coal.Common.Environment (Environment)
import Coal.Kernel.LLVM
import Coal.Kernel.Language.Type (Type)
import Extras (Over)

data PipelineState = PipelineState
  { pipelineSupply :: Int
  , pipelineInterpreterEnv :: IRInterpreterEnv
  , pipelineArtifacts :: [IRInterpreterArtifact]
  , pipelineCode :: [IRConstruct [IRLine]]
  , pipelineNames :: Environment Type
  , pipelineIRTypes :: Environment IRType
  , pipelineConstructors :: Environment Int
  }
  deriving (Show, Eq, Ord)

{-# INLINE initialPipelineState #-}
initialPipelineState :: PipelineState
initialPipelineState = PipelineState 0 (IRInterpreterEnv mempty mempty mempty) [] [] mempty mempty mempty

resetPipelineState :: PipelineState -> PipelineState
resetPipelineState PipelineState{..} =
  PipelineState
    { pipelineSupply = 0
    , pipelineInterpreterEnv = IRInterpreterEnv mempty mempty pipelineIRTypes
    , pipelineArtifacts = []
    , pipelineCode = []
    , ..
    }

{-# INLINE overPipelineStateSupply #-}
overPipelineStateSupply :: Over PipelineState Int
overPipelineStateSupply f PipelineState{..} = PipelineState{pipelineSupply = f pipelineSupply, ..}

{-# INLINE overPipelineStateInterpreterEnv #-}
overPipelineStateInterpreterEnv :: Over PipelineState IRInterpreterEnv
overPipelineStateInterpreterEnv f PipelineState{..} = PipelineState{pipelineInterpreterEnv = f pipelineInterpreterEnv, ..}

{-# INLINE overPipelineStateInterpreterValueEnv #-}
overPipelineStateInterpreterValueEnv :: Over PipelineState (Environment IRValue)
overPipelineStateInterpreterValueEnv = overPipelineStateInterpreterEnv . inValueEnv

{-# INLINE overPipelineStateInterpreterConstructorEnv #-}
overPipelineStateInterpreterConstructorEnv :: Over PipelineState (Environment Int)
overPipelineStateInterpreterConstructorEnv = overPipelineStateInterpreterEnv . inConstructorEnv

{-# INLINE overPipelineStateInterpreterIRTypes #-}
overPipelineStateInterpreterIRTypes :: Over PipelineState (Environment IRType)
overPipelineStateInterpreterIRTypes = overPipelineStateInterpreterEnv . inIRTypes

{-# INLINE overPipelineStateArtifacts #-}
overPipelineStateArtifacts :: Over PipelineState [IRInterpreterArtifact]
overPipelineStateArtifacts f PipelineState{..} = PipelineState{pipelineArtifacts = f pipelineArtifacts, ..}

{-# INLINE overPipelineStateCode #-}
overPipelineStateCode :: Over PipelineState [IRConstruct [IRLine]]
overPipelineStateCode f PipelineState{..} = PipelineState{pipelineCode = f pipelineCode, ..}

{-# INLINE overPipelineStateNames #-}
overPipelineStateNames :: Over PipelineState (Environment Type)
overPipelineStateNames f PipelineState{..} = PipelineState{pipelineNames = f pipelineNames, ..}

{-# INLINE overPipelineStateIRTypes #-}
overPipelineStateIRTypes :: Over PipelineState (Environment IRType)
overPipelineStateIRTypes f PipelineState{..} = PipelineState{pipelineIRTypes = f pipelineIRTypes, ..}

{-# INLINE overPipelineStateConstructors #-}
overPipelineStateConstructors :: Over PipelineState (Environment Int)
overPipelineStateConstructors f PipelineState{..} = PipelineState{pipelineConstructors = f pipelineConstructors, ..}
