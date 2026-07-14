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
