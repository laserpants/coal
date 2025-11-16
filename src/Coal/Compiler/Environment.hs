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

import Coal.Ast.Metadata (Metadata (..))
import Coal.Common.Environment (Environment (..))
import Coal.Compiler.Module.Bundle
import Coal.Language
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
  KernelEnvironment{kernelEnvironmentModule = fn kernelEnvironmentModule, ..}

overKernelEnvironmentLocalNames :: Over KernelEnvironment (Set Name)
overKernelEnvironmentLocalNames fn KernelEnvironment{..} =
  KernelEnvironment{kernelEnvironmentLocalNames = fn kernelEnvironmentLocalNames, ..}

overKernelEnvironmentQualifiedNames :: Over KernelEnvironment (Environment Name)
overKernelEnvironmentQualifiedNames fn KernelEnvironment{..} =
  KernelEnvironment{kernelEnvironmentQualifiedNames = fn kernelEnvironmentQualifiedNames, ..}

data CompilerEnvironment = CompilerEnvironment
  { compilerDataConstructorEnvironment :: Environment (DataConstructorInfo Metadata)
  , compilerCodataAccessorEnvironment :: Environment (CodataAccessorInfo Metadata)
  , compilerTypeConstructorEnvironment :: Environment Kind
  , compilerTraitEnvironment :: Environment (TraitInfo Metadata)
  , compilerInstanceEnvironment :: Environment (Map IndexedType (InstanceInfo Metadata))
  , compilerAliasEnvironment :: Environment (AliasInfo Metadata)
  , compilerDictionaryNameEnvironment :: Environment IndexedScheme
  , compilerKernelEnvironment :: KernelEnvironment
  }
  deriving (Show, Eq, Ord, Read)

emptyCompilerEnvironment :: CompilerEnvironment
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
  CompilerEnvironment ->
  CompilerEnvironment
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
  CompilerEnvironment ->
  CompilerEnvironment
overCompilerKernelEnvironment f CompilerEnvironment{..} =
  CompilerEnvironment
    { compilerKernelEnvironment =
        f compilerKernelEnvironment
    , ..
    }
