{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.Compiler.Kernel (
  Kernel (..),
  initialKernel,
  overKernelSupply,
  overKernelInterpreterEnv,
  overKernelInterpreterValueEnv,
  overKernelInterpreterConstructorEnv,
  overKernelArtifacts,
  overKernelCode,
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

data Kernel = Kernel
  { pipelineSupply :: Int
  , pipelineInterpreterEnv :: IRInterpreterEnv
  , pipelineArtifacts :: [IRInterpreterArtifact]
  , pipelineCode :: [IRConstruct [IRLine]]
  }
  deriving (Show, Eq, Ord)

{-# INLINE initialKernel #-}
initialKernel :: Kernel
initialKernel = Kernel 0 (IRInterpreterEnv mempty mempty) [] []

{-# INLINE overKernelSupply #-}
overKernelSupply :: Over Kernel Int
overKernelSupply f Kernel{..} = Kernel{pipelineSupply = f pipelineSupply, ..}

{-# INLINE overKernelInterpreterEnv #-}
overKernelInterpreterEnv :: Over Kernel IRInterpreterEnv
overKernelInterpreterEnv f Kernel{..} = Kernel{pipelineInterpreterEnv = f pipelineInterpreterEnv, ..}

{-# INLINE overKernelInterpreterValueEnv #-}
overKernelInterpreterValueEnv :: Over Kernel (Environment IRValue)
overKernelInterpreterValueEnv = overKernelInterpreterEnv . inValueEnv

{-# INLINE overKernelInterpreterConstructorEnv #-}
overKernelInterpreterConstructorEnv :: Over Kernel (Environment Int)
overKernelInterpreterConstructorEnv = overKernelInterpreterEnv . inConstructorEnv

{-# INLINE overKernelArtifacts #-}
overKernelArtifacts :: Over Kernel [IRInterpreterArtifact]
overKernelArtifacts f Kernel{..} = Kernel{pipelineArtifacts = f pipelineArtifacts, ..}

{-# INLINE overKernelCode #-}
overKernelCode :: Over Kernel [IRConstruct [IRLine]]
overKernelCode f Kernel{..} = Kernel{pipelineCode = f pipelineCode, ..}
