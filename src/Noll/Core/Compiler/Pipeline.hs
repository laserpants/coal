{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.Compiler.Pipeline (
  Pipeline (..),
  initialPipeline,
  overPipelineSupply,
  overPipelineInterpreterEnv,
  overPipelineInterpreterValueEnv,
  overPipelineInterpreterConstructorEnv,
  overPipelineArtifacts,
  overPipelineCode,
) where

import Noll.Common.Environment (Environment)
import Noll.Core.LLVM.IRConstruct (IRConstruct (..))
import Noll.Core.LLVM.IRInterpreter.Artifact (IRInterpreterArtifact)
import Noll.Core.LLVM.IRInterpreter.Environment (
  IRInterpreterEnv (..),
  inConstructorEnv,
  inValueEnv,
 )
import Noll.Core.LLVM.IRInterpreter.Monad (IRLine (..))
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Utils (Over)

data Pipeline = Pipeline
  { pipelineStateSupply :: Int
  , pipelineStateInterpreterEnv :: IRInterpreterEnv
  , pipelineStateArtifacts :: [IRInterpreterArtifact]
  , pipelineStateCode :: [IRConstruct [IRLine]]
  }
  deriving (Show, Eq, Ord)

{-# INLINE initialPipeline #-}
initialPipeline :: Pipeline
initialPipeline = Pipeline 0 (IRInterpreterEnv mempty mempty) [] []

{-# INLINE overPipelineSupply #-}
overPipelineSupply :: Over Pipeline Int
overPipelineSupply f Pipeline{..} = Pipeline{pipelineStateSupply = f pipelineStateSupply, ..}

{-# INLINE overPipelineInterpreterEnv #-}
overPipelineInterpreterEnv :: Over Pipeline IRInterpreterEnv
overPipelineInterpreterEnv f Pipeline{..} = Pipeline{pipelineStateInterpreterEnv = f pipelineStateInterpreterEnv, ..}

{-# INLINE overPipelineInterpreterValueEnv #-}
overPipelineInterpreterValueEnv :: Over Pipeline (Environment IRValue)
overPipelineInterpreterValueEnv = overPipelineInterpreterEnv . inValueEnv

{-# INLINE overPipelineInterpreterConstructorEnv #-}
overPipelineInterpreterConstructorEnv :: Over Pipeline (Environment Int)
overPipelineInterpreterConstructorEnv = overPipelineInterpreterEnv . inConstructorEnv

{-# INLINE overPipelineArtifacts #-}
overPipelineArtifacts :: Over Pipeline [IRInterpreterArtifact]
overPipelineArtifacts f Pipeline{..} = Pipeline{pipelineStateArtifacts = f pipelineStateArtifacts, ..}

{-# INLINE overPipelineCode #-}
overPipelineCode :: Over Pipeline [IRConstruct [IRLine]]
overPipelineCode f Pipeline{..} = Pipeline{pipelineStateCode = f pipelineStateCode, ..}
