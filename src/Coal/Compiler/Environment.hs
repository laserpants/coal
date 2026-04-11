{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Environment (
  CompilerEnvironment (..),
  KernelEnvironment (..),
  emptyCompilerEnvironment,
  overCompilerKernelEnvironment,
  overKernelEnvironmentModule,
  overKernelEnvironmentLocalNames,
  overKernelEnvironmentQualifiedNames,
) where

import Coal.Common.Environment (Environment (..))
import Extras (Name, Over, Set)
import System.Console.AsciiProgress

data KernelEnvironment = KernelEnvironment
  { kernelEnvironmentModule :: Name
  , kernelEnvironmentLocalNames :: Set Name
  , kernelEnvironmentQualifiedNames :: Environment Name
  }
  deriving (Show, Eq, Ord, Read)

overKernelEnvironmentModule :: Over KernelEnvironment Name
overKernelEnvironmentModule fn KernelEnvironment{..} =
  KernelEnvironment
    { kernelEnvironmentModule =
        fn kernelEnvironmentModule
    , ..
    }

overKernelEnvironmentLocalNames :: Over KernelEnvironment (Set Name)
overKernelEnvironmentLocalNames fn KernelEnvironment{..} =
  KernelEnvironment
    { kernelEnvironmentLocalNames =
        fn kernelEnvironmentLocalNames
    , ..
    }

overKernelEnvironmentQualifiedNames :: Over KernelEnvironment (Environment Name)
overKernelEnvironmentQualifiedNames fn KernelEnvironment{..} =
  KernelEnvironment
    { kernelEnvironmentQualifiedNames =
        fn kernelEnvironmentQualifiedNames
    , ..
    }

data CompilerEnvironment a = CompilerEnvironment
  { compilerKernelEnvironment :: KernelEnvironment
  , compilerProgressBar :: Maybe ProgressBar
  }

emptyCompilerEnvironment :: Maybe ProgressBar -> CompilerEnvironment a
emptyCompilerEnvironment progressBar =
  CompilerEnvironment
    { compilerKernelEnvironment = KernelEnvironment mempty mempty mempty
    , compilerProgressBar = progressBar
    }

overCompilerKernelEnvironment ::
  ( KernelEnvironment ->
    KernelEnvironment
  ) ->
  CompilerEnvironment a ->
  CompilerEnvironment a
overCompilerKernelEnvironment f CompilerEnvironment{..} =
  CompilerEnvironment
    { compilerKernelEnvironment =
        f compilerKernelEnvironment
    , ..
    }
