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
  overPipelineStateArtifacts,
  overPipelineStateCode,
  overPipelineStateNames,
  overPipelineStateIRTypes,
  overPipelineStateConstructors,
) where

import Coal.Common.Environment (Environment)
import Coal.Kernel.LLVM
import Coal.Kernel.Language.Type (Type)
import Extra (Over)

data PipelineState = PipelineState
  { kernelSupply :: Int
  , kernelInterpreterEnv :: IRInterpreterEnv
  , kernelArtifacts :: [IRInterpreterArtifact]
  , kernelCode :: [IRConstruct [IRLine]]
  , kernelNames :: Environment Type
  , kernelIRTypes :: Environment IRType
  , kernelConstructors :: Environment Int
  }
  deriving (Show, Eq, Ord)

{-# INLINE initialPipelineState #-}
initialPipelineState :: PipelineState
initialPipelineState = PipelineState 0 (IRInterpreterEnv mempty mempty) [] [] mempty mempty mempty

resetPipelineState :: PipelineState -> PipelineState
resetPipelineState PipelineState{..} =
  PipelineState
    { kernelSupply = 0
    , kernelInterpreterEnv = IRInterpreterEnv mempty mempty
    , kernelArtifacts = []
    , kernelCode = []
    , ..
    }

{-# INLINE overPipelineStateSupply #-}
overPipelineStateSupply :: Over PipelineState Int
overPipelineStateSupply f PipelineState{..} = PipelineState{kernelSupply = f kernelSupply, ..}

{-# INLINE overPipelineStateInterpreterEnv #-}
overPipelineStateInterpreterEnv :: Over PipelineState IRInterpreterEnv
overPipelineStateInterpreterEnv f PipelineState{..} = PipelineState{kernelInterpreterEnv = f kernelInterpreterEnv, ..}

{-# INLINE overPipelineStateInterpreterValueEnv #-}
overPipelineStateInterpreterValueEnv :: Over PipelineState (Environment IRValue)
overPipelineStateInterpreterValueEnv = overPipelineStateInterpreterEnv . inValueEnv

{-# INLINE overPipelineStateInterpreterConstructorEnv #-}
overPipelineStateInterpreterConstructorEnv :: Over PipelineState (Environment Int)
overPipelineStateInterpreterConstructorEnv = overPipelineStateInterpreterEnv . inConstructorEnv

{-# INLINE overPipelineStateArtifacts #-}
overPipelineStateArtifacts :: Over PipelineState [IRInterpreterArtifact]
overPipelineStateArtifacts f PipelineState{..} = PipelineState{kernelArtifacts = f kernelArtifacts, ..}

{-# INLINE overPipelineStateCode #-}
overPipelineStateCode :: Over PipelineState [IRConstruct [IRLine]]
overPipelineStateCode f PipelineState{..} = PipelineState{kernelCode = f kernelCode, ..}

{-# INLINE overPipelineStateNames #-}
overPipelineStateNames :: Over PipelineState (Environment Type)
overPipelineStateNames f PipelineState{..} = PipelineState{kernelNames = f kernelNames, ..}

{-# INLINE overPipelineStateIRTypes #-}
overPipelineStateIRTypes :: Over PipelineState (Environment IRType)
overPipelineStateIRTypes f PipelineState{..} = PipelineState{kernelIRTypes = f kernelIRTypes, ..}

{-# INLINE overPipelineStateConstructors #-}
overPipelineStateConstructors :: Over PipelineState (Environment Int)
overPipelineStateConstructors f PipelineState{..} = PipelineState{kernelConstructors = f kernelConstructors, ..}
