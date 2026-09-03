{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CLI.Command.Build (buildCommand, deriveExecutableName, resolveExecutableName) where

import CLI.Command.EntryPoint (parseEntryPoint)
import CLI.Error (CLIError (..))
import Coal.Compiler (compile)
import Coal.Compiler.Config
import Coal.Compiler.Terminal (TerminalCapabilities)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.SemVer (toText)
import Data.Text (Text)
import qualified Data.Text as Text
import Extras (forM)
import Package (packageIncludes)
import Package.Error (PackageError (..))
import Package.Manifest (BuildConfig (..), PackageManifest (..), filePaths, loadProjectManifest)
import Package.Version (PackageVersion (..))
import System.Directory (canonicalizePath)

buildCommand :: TerminalCapabilities -> ExceptT CLIError IO ()
buildCommand caps = do
  res <- liftIO $ runExceptT $ do
    PackageManifest{..} <- loadProjectManifest

    -- If there is no lock file but the project declares no dependencies,
    -- treat it as zero packages rather than requiring `coal install`.
    pkgResult <- lift $ runExceptT packageIncludes
    (nsInfo, inputFiles, pkgCFiles) <- case pkgResult of
      Right result ->
        pure result
      Left ENoLockFile
        | noDependencies dependencies ->
            pure ([], [], [])
      Left err ->
        throwError err
    -- Canonicalize package source dirs so they match the canonical bestRoot
    -- returned by resolveModule inside the parsing pass.
    canonNsInfo <- forM nsInfo $
      \(dir, ns, mods) -> do
        canonDir <- liftIO $ canonicalizePath dir
        pure (canonDir, ns, mods)
    -- Resolve the project's own C sources (declared in coal.json) relative to
    -- the current directory so the linker can compile and link them.
    localCFiles <- liftIO $ mapM canonicalizePath (fromMaybe [] c_sources)
    let localSrcPaths = Text.unpack <$> fromMaybe ["src"] source_dirs
        pkgSrcPaths = [d | (d, _, _) <- canonNsInfo]
        entryPoint = parseEntryPoint entry_point
        execName = resolveExecutableName name version executable_name
        baseConfig =
          defaultConfig
            { -- Local source dirs first so project modules shadow same-named package modules.
              configSourcePaths = nub ("src" : localSrcPaths <> pkgSrcPaths)
            , configExecutableName = execName
            , configEntryPoint = entryPoint
            , configPackageNamespaces = canonNsInfo
            , -- C sources contributed by the project itself and by installed packages
              -- (e.g. an EventSource library shipping its native primitives).
              configCFiles = localCFiles <> pkgCFiles
            }
        config = applyBuildConfig build_config baseConfig
    localFiles <- filePaths modules EProjectInvalidModuleFormat
    -- If the entry-point module is not listed in `modules`, add its file
    -- automatically so the user doesn't have to duplicate it there.
    let extraModules = filter (`notElem` modules) (maybeToList (fst <$> entryPoint))
    extraFiles <- filePaths extraModules EProjectInvalidModuleFormat
    pure (config, inputFiles <> nub (localFiles <> extraFiles))

  case res of
    Left err ->
      throwError (EPackageError err)
    Right (config, files) ->
      liftIO $ compile caps config files

-- | True when the manifest declares no external dependencies.
noDependencies :: Maybe (Map Text a) -> Bool
noDependencies = maybe True Map.null

-- | Derive the executable name from the project name and optional version.
deriveExecutableName :: Text -> Maybe PackageVersion -> FilePath
deriveExecutableName projectName =
  \case
    Just (PackageVersion v) -> Text.unpack projectName <> "-" <> Text.unpack (toText v)
    Nothing -> Text.unpack projectName

{- | Resolve the final executable name, preferring an explicit @executable_name@
override from the manifest over the derived @name-version@ form.
-}
resolveExecutableName :: Text -> Maybe PackageVersion -> Maybe FilePath -> FilePath
resolveExecutableName projectName version = fromMaybe (deriveExecutableName projectName version)

{- | Apply a @BuildConfig@ from the manifest to a @CompilerConfig@, overriding
the default values. When the manifest has no @build@ section, the config
is unchanged.
-}
applyBuildConfig :: Maybe BuildConfig -> CompilerConfig -> CompilerConfig
applyBuildConfig Nothing = id
applyBuildConfig (Just BuildConfig{..}) =
  \cfg ->
    cfg
      { configGenerateDebugArtifacts = generateDebugArtifacts
      , configGenerateLLVMOutput = debugLLVMOutput
      , configSilent = silent
      , configShowTiming = showTiming
      , configNoCache = noCache
      , configSanitize = sanitize
      }
