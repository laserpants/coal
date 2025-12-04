{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Environment (
  CompilerEnvironment (..),
  KernelEnvironment (..),
  emptyCompilerEnvironment,
  overCompilerDictionaryNameEnvironment,
  overCompilerKernelEnvironment,
  overKernelEnvironmentModule,
  overKernelEnvironmentLocalNames,
  overKernelEnvironmentQualifiedNames,
) where

import Coal.Common.Environment (Environment (..))
import Coal.Compiler.Build
import Coal.Language (IndexedScheme, IndexedType, Kind)
import Data.Map.Strict (Map)
import Extras (Name, Over, Set)

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
  { compilerDataConstructorEnvironment :: Environment (DataConstructorEntry a)
  , compilerCodataAccessorEnvironment :: Environment (CodataAccessorEntry a)
  , compilerTypeConstructorEnvironment :: Environment Kind
  , compilerTraitEnvironment :: Environment (TraitEntry a)
  , compilerInstanceEnvironment :: Environment (Map IndexedType (InstanceEntry a))
  , compilerAliasEnvironment :: Environment (AliasEntry a)
  , compilerDictionaryNameEnvironment :: Environment IndexedScheme
  , compilerKernelEnvironment :: KernelEnvironment
  }
  deriving (Show, Eq, Ord, Read)

emptyCompilerEnvironment :: CompilerEnvironment a
emptyCompilerEnvironment =
  CompilerEnvironment
    { compilerDataConstructorEnvironment = mempty
    , compilerCodataAccessorEnvironment = mempty
    , compilerTypeConstructorEnvironment = mempty
    , compilerTraitEnvironment = mempty
    , compilerInstanceEnvironment = mempty
    , compilerAliasEnvironment = mempty
    , compilerDictionaryNameEnvironment = mempty
    , compilerKernelEnvironment = KernelEnvironment mempty mempty mempty
    }

overCompilerDictionaryNameEnvironment ::
  ( Environment IndexedScheme ->
    Environment IndexedScheme
  ) ->
  CompilerEnvironment a ->
  CompilerEnvironment a
overCompilerDictionaryNameEnvironment f CompilerEnvironment{..} =
  CompilerEnvironment
    { compilerDictionaryNameEnvironment =
        f compilerDictionaryNameEnvironment
    , ..
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
