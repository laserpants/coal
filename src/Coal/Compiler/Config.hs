{-# LANGUAGE NamedFieldPuns #-}
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
  setConfigSanitize,
  configHash,
) where

import Coal.Compiler.Build.Hash256 (Hash256 (..))
import Crypto.Hash (hash)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Extras (Name)
import System.FilePath (takeFileName)

data CompilerConfig = CompilerConfig
  { configExecutableName :: FilePath
  , configGenerateDebugArtifacts :: Bool
  , configGenerateLLVMOutput :: Bool
  , configSourcePaths :: [FilePath]
  , configCFiles :: [FilePath]
  , configSilent :: Bool
  , configShowTiming :: Bool
  , configNoCache :: Bool
  , configEntryPoint :: Maybe (Name, Name)
  , configPackageNamespaces :: [(FilePath, Text, [Name])]
  {- ^ Each triple: (canonical source dir, namespace prefix, unqualified module names).
  Package modules from a given source dir are renamed as @namespace.ModuleName@.
  -}
  , configSanitize :: Bool
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
    , configShowTiming = False
    , configNoCache = False
    , configEntryPoint = Nothing
    , configPackageNamespaces = []
    , configSanitize = False
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

{-# INLINE setConfigSanitize #-}
setConfigSanitize :: Bool -> CompilerConfig -> CompilerConfig
setConfigSanitize flag CompilerConfig{..} =
  CompilerConfig
    { configSanitize =
        flag
    , ..
    }

{- | Version marker for the on-disk Binary encoding of build artifacts.

Bump whenever any type serialized into the build cache changes shape (e.g. a
field is added to an entry type) so that stale cache files are invalidated
instead of being decoded misaligned.

History:

  * @"4"@: 'configHash' now also covers @configEntryPoint@, @configCFiles@ and
    @configSanitize@, all of which affect generated code or linked output.
    Cached builds written under the old hash must not be reused, as they can
    silently link the wrong entry point or unsanitized objects.
  * @"3"@: step 7b now copies stdlib instance member name schemes into the
    consuming module's name store; stale cached builds lack these entries and
    would no longer resolve context-carrying member schemes.
  * @"2"@ — added @instanceEntryModule@ to @InstanceEntry@.
-}
buildCacheFormatVersion :: Text
buildCacheFormatVersion = "4"

{- | Compute a hash of the configuration fields that affect compilation output.
Used to invalidate cached builds when relevant config changes.

Only fields that change generated code or linked output belong here. Note in
particular that @configEntryPoint@ (which module receives the C @main@) and
@configCFiles@ (extra objects linked into the binary) are not diagnostic
settings: a cached build produced under different values is wrong, not merely
stale, so omitting them yields silent miscompilation rather than a rebuild.

@configCFiles@ holds canonicalized absolute paths, so only the basenames are
hashed; hashing the full paths would make the cache machine-specific and
prevent sharing between checkouts. This detects adding or removing a C file
but not editing its contents.

@configSourcePaths@ is excluded for the same reason (it holds absolute
paths), and is unnecessary in any case: the resolved source /text/ of each
module is already covered by the per-module source hash checked by
'Coal.Compiler.Build.Cache.cachedBuild'.
-}
configHash :: CompilerConfig -> Hash256
configHash cfg =
  Hash256 (hash (encodeUtf8 (pack (show hashedFields) <> buildCacheFormatVersion)))
 where
  CompilerConfig{..} = cfg
  -- Deliberately excluded, because they cannot change generated code:
  -- configExecutableName (output filename only), configSilent,
  -- configShowTiming, configGenerateDebugArtifacts, configGenerateLLVMOutput
  -- and configNoCache.
  hashedFields =
    ( configPackageNamespaces
    , configEntryPoint
    , map takeFileName configCFiles
    , configSanitize
    )
