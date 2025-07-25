{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.Kernel.Compiler.Pipeline.Kernel (
  Kernel (..),
  initialKernel,
  resetKernel,
  overKernelSupply,
  overKernelInterpreterEnv,
  overKernelInterpreterValueEnv,
  overKernelInterpreterConstructorEnv,
  overKernelArtifacts,
  overKernelCode,
  overKernelNames,
  overKernelIRTypes,
  overKernelConstructors,
) where

import Lang.Common.Environment (Environment)
import Noll.Kernel.LLVM
import Noll.Kernel.Language.Type (Type)
import Extra (Over)

data Kernel = Kernel
  { kernelSupply :: Int
  , kernelInterpreterEnv :: IRInterpreterEnv
  , kernelArtifacts :: [IRInterpreterArtifact]
  , kernelCode :: [IRConstruct [IRLine]]
  , kernelNames :: Environment Type
  , kernelIRTypes :: Environment IRType
  , kernelConstructors :: Environment Int
  }
  deriving (Show, Eq, Ord)

{-# INLINE initialKernel #-}
initialKernel :: Kernel
initialKernel = Kernel 0 (IRInterpreterEnv mempty mempty) [] [] mempty mempty mempty

resetKernel :: Kernel -> Kernel
resetKernel Kernel{..} =
  Kernel
    { kernelSupply = 0
    , kernelInterpreterEnv = IRInterpreterEnv mempty mempty
    , kernelArtifacts = []
    , kernelCode = []
    , ..
    }

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

{-# INLINE overKernelNames #-}
overKernelNames :: Over Kernel (Environment Type)
overKernelNames f Kernel{..} = Kernel{kernelNames = f kernelNames, ..}

{-# INLINE overKernelIRTypes #-}
overKernelIRTypes :: Over Kernel (Environment IRType)
overKernelIRTypes f Kernel{..} = Kernel{kernelIRTypes = f kernelIRTypes, ..}

{-# INLINE overKernelConstructors #-}
overKernelConstructors :: Over Kernel (Environment Int)
overKernelConstructors f Kernel{..} = Kernel{kernelConstructors = f kernelConstructors, ..}
