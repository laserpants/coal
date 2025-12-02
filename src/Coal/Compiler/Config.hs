{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Config (
  CompilerConfig (..),
  defaultConfig,
  silentConfig,
  setConfigExecutableName,
  setConfigGenerateDotFiles,
  setConfigGenerateLLVMOutput,
) where

data CompilerConfig = CompilerConfig
  { configExecutableName :: FilePath
  , configGenerateDotFiles :: Bool
  , configGenerateLLVMOutput :: Bool
  , configCFiles :: [FilePath]
  , configSilent :: Bool
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE defaultConfig #-}
defaultConfig :: CompilerConfig
defaultConfig =
  CompilerConfig
    { configExecutableName = "dist"
    , configGenerateDotFiles = True
    , configGenerateLLVMOutput = True
    , configCFiles = []
    , configSilent = False
    }

{-# INLINE silentConfig #-}
silentConfig :: CompilerConfig
silentConfig = defaultConfig{ configSilent = True }

{-# INLINE setConfigExecutableName #-}
setConfigExecutableName :: FilePath -> CompilerConfig -> CompilerConfig
setConfigExecutableName name CompilerConfig{..} =
  CompilerConfig{configExecutableName = name, ..}

{-# INLINE setConfigGenerateDotFiles #-}
setConfigGenerateDotFiles :: Bool -> CompilerConfig -> CompilerConfig
setConfigGenerateDotFiles flag CompilerConfig{..} =
  CompilerConfig{configGenerateDotFiles = flag, ..}

{-# INLINE setConfigGenerateLLVMOutput #-}
setConfigGenerateLLVMOutput :: Bool -> CompilerConfig -> CompilerConfig
setConfigGenerateLLVMOutput flag CompilerConfig{..} =
  CompilerConfig{configGenerateLLVMOutput = flag, ..}
