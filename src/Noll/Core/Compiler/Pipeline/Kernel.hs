{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.Compiler.Pipeline.Kernel (
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
  { kernelSupply :: Int
  , kernelInterpreterEnv :: IRInterpreterEnv
  , kernelArtifacts :: [IRInterpreterArtifact]
  , kernelCode :: [IRConstruct [IRLine]]
  }
  deriving (Show, Eq, Ord)

{-# INLINE initialKernel #-}
initialKernel :: Kernel
initialKernel = Kernel 0 (IRInterpreterEnv mempty mempty) [] []

{-# INLINE overKernelSupply #-}
overKernelSupply :: Over Kernel Int
overKernelSupply f Kernel{..} = Kernel{kernelSupply = f kernelSupply, ..}

{-# INLINE overKernelInterpreterEnv #-}
overKernelInterpreterEnv :: Over Kernel IRInterpreterEnv
overKernelInterpreterEnv f Kernel{..} = Kernel{kernelInterpreterEnv = f kernelInterpreterEnv, ..}

{-# INLINE overKernelInterpreterValueEnv #-}
overKernelInterpreterValueEnv :: Over Kernel (Environment IRValue)
overKernelInterpreterValueEnv = overKernelInterpreterEnv . inValueEnv

{-# INLINE overKernelInterpreterConstructorEnv #-}
overKernelInterpreterConstructorEnv :: Over Kernel (Environment Int)
overKernelInterpreterConstructorEnv = overKernelInterpreterEnv . inConstructorEnv

{-# INLINE overKernelArtifacts #-}
overKernelArtifacts :: Over Kernel [IRInterpreterArtifact]
overKernelArtifacts f Kernel{..} = Kernel{kernelArtifacts = f kernelArtifacts, ..}

{-# INLINE overKernelCode #-}
overKernelCode :: Over Kernel [IRConstruct [IRLine]]
overKernelCode f Kernel{..} = Kernel{kernelCode = f kernelCode, ..}
