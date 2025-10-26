{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Config (
  CompilerConfig (..),
  defaultConfig,
  setConfigExecutableName,
  setConfigGenerateDotFiles,
) where

data CompilerConfig = CompilerConfig
  { configExecutableName :: FilePath
  , configGenerateDotFiles :: Bool
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE defaultConfig #-}
defaultConfig :: CompilerConfig
defaultConfig =
  CompilerConfig
    { configExecutableName = "dist"
    , configGenerateDotFiles = True
    }

{-# INLINE setConfigExecutableName #-}
setConfigExecutableName :: FilePath -> CompilerConfig -> CompilerConfig
setConfigExecutableName name CompilerConfig{..} = CompilerConfig{configExecutableName = name, ..}

{-# INLINE setConfigGenerateDotFiles #-}
setConfigGenerateDotFiles :: Bool -> CompilerConfig -> CompilerConfig
setConfigGenerateDotFiles flag CompilerConfig{..} = CompilerConfig{configGenerateDotFiles = flag, ..}
