{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Compiler.Config
Description: Compiler configuration settings

This module defines the compiler configuration and provides default
configurations for various compilation scenarios.
-}
module Coal.Compiler.Config (
  CompilerConfig (..),
  defaultConfig,
  silentConfig,
  debugConfig,
  setConfigExecutableName,
  setConfigGenerateDebugArtifacts,
  setConfigGenerateLLVMOutput,
) where

import Data.Text (Text)
import Extras (Name)

data CompilerConfig = CompilerConfig
  { configExecutableName :: FilePath
  , configGenerateDebugArtifacts :: Bool
  , configGenerateLLVMOutput :: Bool
  , configSourcePaths :: [FilePath]
  , configCFiles :: [FilePath]
  , configSilent :: Bool
  , configNoCache :: Bool
  , configEntryPoint :: Maybe (Name, Name)
  , configPackageNamespaces :: [(FilePath, Text, [Name])]
  -- ^ Each triple: (canonical source dir, namespace prefix, unqualified module names).
  -- Package modules from a given source dir are renamed as @namespace.ModuleName@.
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE defaultConfig #-}
defaultConfig :: CompilerConfig
defaultConfig =
  CompilerConfig
    { configExecutableName = "dist"
    , configGenerateDebugArtifacts = False
    , configGenerateLLVMOutput = False
    , configSourcePaths = ["src"]
    , configCFiles = []
    , configSilent = False
    , configNoCache = False
    , configEntryPoint = Nothing
    , configPackageNamespaces = []
    }

{-# INLINE silentConfig #-}
silentConfig :: CompilerConfig
silentConfig = defaultConfig{configSilent = True, configEntryPoint = Nothing}

{-# INLINE debugConfig #-}
debugConfig :: CompilerConfig
debugConfig =
  defaultConfig
    { configNoCache = True
    , configGenerateDebugArtifacts = True
    , configGenerateLLVMOutput = True
    , configEntryPoint = Nothing
    }

{-# INLINE setConfigExecutableName #-}
setConfigExecutableName :: FilePath -> CompilerConfig -> CompilerConfig
setConfigExecutableName name CompilerConfig{..} =
  CompilerConfig
    { configExecutableName =
        name
    , ..
    }

{-# INLINE setConfigGenerateDebugArtifacts #-}
setConfigGenerateDebugArtifacts :: Bool -> CompilerConfig -> CompilerConfig
setConfigGenerateDebugArtifacts flag CompilerConfig{..} =
  CompilerConfig
    { configGenerateDebugArtifacts =
        flag
    , ..
    }

{-# INLINE setConfigGenerateLLVMOutput #-}
setConfigGenerateLLVMOutput :: Bool -> CompilerConfig -> CompilerConfig
setConfigGenerateLLVMOutput flag CompilerConfig{..} =
  CompilerConfig
    { configGenerateLLVMOutput =
        flag
    , ..
    }
