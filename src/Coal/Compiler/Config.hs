-- +
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
  setConfigGenerateDotFiles,
  setConfigGenerateLLVMOutput,
) where

data CompilerConfig = CompilerConfig
  { configExecutableName :: FilePath
  , configGenerateDotFiles :: Bool
  , configGenerateLLVMOutput :: Bool
  , configSourcePaths :: [FilePath]
  , configCFiles :: [FilePath]
  , configSilent :: Bool
  , configNoCache :: Bool
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE defaultConfig #-}
defaultConfig :: CompilerConfig
defaultConfig =
  CompilerConfig
    { configExecutableName = "dist"
    , configGenerateDotFiles = True
    , configGenerateLLVMOutput = True
    , configSourcePaths = ["src"]
    , configCFiles = []
    , configSilent = False
    , configNoCache = False
    }

{-# INLINE silentConfig #-}
silentConfig :: CompilerConfig
silentConfig = defaultConfig{configSilent = True}

{-# INLINE debugConfig #-}
debugConfig :: CompilerConfig
debugConfig = defaultConfig{configNoCache = True}

{-# INLINE setConfigExecutableName #-}
setConfigExecutableName :: FilePath -> CompilerConfig -> CompilerConfig
setConfigExecutableName name CompilerConfig{..} =
  CompilerConfig
    { configExecutableName =
        name
    , ..
    }

{-# INLINE setConfigGenerateDotFiles #-}
setConfigGenerateDotFiles :: Bool -> CompilerConfig -> CompilerConfig
setConfigGenerateDotFiles flag CompilerConfig{..} =
  CompilerConfig
    { configGenerateDotFiles =
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
