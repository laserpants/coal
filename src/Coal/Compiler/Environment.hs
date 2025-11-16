{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Environment (
  AliasEnvironment,
  DataConstructorEnvironment,
  TypeConstructorEnvironment,
  TraitEnvironment,
  InstanceEnvironment,
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

type AliasEnvironment = Environment (AliasInfo Metadata) -- ([Name], ParameterizedType)
type DataConstructorEnvironment = Environment (DataConstructorInfo Metadata)
type TypeConstructorEnvironment = Environment Kind
type TraitEnvironment = Environment (TraitInfo Metadata) -- (Parameter Kind, TypeIndex Kind, Environment IndexedScheme)
type InstanceEnvironment = Environment (Map IndexedType (InstanceInfo Metadata))
type CodataAccessorEnvironment = Environment (CodataAccessorInfo Metadata)
type DictionaryNameEnvironment = Environment IndexedScheme

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
  { compilerDataConstructorEnvironment :: DataConstructorEnvironment
  , compilerTypeConstructorEnvironment :: TypeConstructorEnvironment
  , compilerTraitEnvironment :: TraitEnvironment
  , compilerInstanceEnvironment :: InstanceEnvironment
  , compilerAliasEnvironment :: AliasEnvironment
  , compilerCodataAccessorEnvironment :: CodataAccessorEnvironment
  , compilerDictionaryNameEnvironment :: DictionaryNameEnvironment
  , compilerKernelEnvironment :: KernelEnvironment
  }
  deriving (Show, Eq, Ord, Read)

emptyCompilerEnvironment :: CompilerEnvironment
emptyCompilerEnvironment =
  CompilerEnvironment
    { compilerDataConstructorEnvironment = mempty
    , compilerTypeConstructorEnvironment = mempty
    , compilerTraitEnvironment = mempty
    , compilerInstanceEnvironment = mempty
    , compilerAliasEnvironment = mempty
    , compilerCodataAccessorEnvironment = mempty
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
