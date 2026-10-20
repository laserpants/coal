{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CLI.Command.Build (buildCommand, deriveExecutableName, mergeNativeLibraries, resolveExecutableName) where

import CLI.Command.EntryPoint (parseEntryPoint)
import CLI.Error (CLIError (..))
import Coal.Compiler (compile)
import Coal.Compiler.Config
import Coal.Compiler.Path.Resolve (resolveModule)
import Coal.Compiler.Terminal (TerminalCapabilities)
import Control.Monad (forM_)
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
import Extras (Name, forM)
import Package (PackageIncludes (..), emptyPackageIncludes, packageIncludes)
import Package.Error (PackageError (..))
import Package.Manifest (BuildConfig (..), PackageManifest (..), filePaths, loadProjectManifest)
import Package.Version (PackageVersion (..))
import System.Directory (canonicalizePath)
import System.Exit (ExitCode (ExitFailure), exitWith)

buildCommand :: TerminalCapabilities -> ExceptT CLIError IO ()
buildCommand caps = do
  res <- liftIO $ runExceptT $ do
    PackageManifest{..} <- loadProjectManifest

    -- If there is no lock file but the project declares no dependencies,
    -- treat it as zero packages rather than requiring `coal install`.
    pkgResult <- lift $ runExceptT packageIncludes
    includes <- case pkgResult of
      Right result ->
        pure result
      Left ENoLockFile
        | noDependencies dependencies ->
            pure emptyPackageIncludes
      Left err ->
        throwError err
    let nsInfo = packageNamespaces includes
        inputFiles = packageSourceFiles includes
        pkgCFiles = packageCFiles includes
        pkgLibs = packageNativeLibraries includes
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
        -- Search roots for the project's own modules. Package source roots are
        -- deliberately excluded: a module listed in the project manifest must
        -- be found in the project itself, never in a dependency (see below).
        localSearchPaths = nub ("src" : localSrcPaths)
        entryPoint = parseEntryPoint entry_point
        entryModName = fst <$> entryPoint
        -- Every module the project is responsible for compiling locally: those
        -- listed in the manifest plus the (possibly implicit) entry-point module.
        localModuleNames = nub (modules <> maybeToList entryModName)
        execName = resolveExecutableName name version executable_name
        baseConfig =
          defaultConfig
            { configSourcePaths = localSearchPaths
            , configExecutableName = execName
            , configEntryPoint = entryPoint
            , configPackageNamespaces = canonNsInfo
            , -- Locally-defined modules shadow same-named package modules.
              configLocalModules = localModuleNames
            , -- C sources contributed by the project itself and by installed packages
              -- (e.g. an EventSource library shipping its native primitives).
              configCFiles = localCFiles <> pkgCFiles
            , -- Native libraries declared by the project and by installed packages
              -- (including transitive dependencies), linked as -l<name>.
              configNativeLibraries = mergeNativeLibraries native_libraries pkgLibs
            }
        config = applyBuildConfig build_config baseConfig
    localFiles <- filePaths localModuleNames EProjectInvalidModuleFormat
    -- Every locally-declared module must exist inside the project's own source
    -- directories. Without this check a missing local module silently resolves
    -- to a same-named file inside a dependency, which both shadows the intended
    -- local module and compiles the dependency's file twice (producing duplicate
    -- linker symbols). Report the misconfiguration instead.
    forM_ (zip localModuleNames localFiles) $ \(modName, relFile) -> do
      resolved <- liftIO $ resolveModule localSearchPaths localSearchPaths relFile
      case resolved of
        Right{} -> pure ()
        Left{} ->
          throwError (EProjectModuleNotFound modName (packageProvidersOf canonNsInfo modName))
    pure (config, inputFiles <> nub localFiles)

  case res of
    Left err ->
      throwError (EPackageError err)
    Right (config, files) -> do
      result <- liftIO $ compile caps config files
      case result of
        Left{} ->
          liftIO $ exitWith (ExitFailure 1)
        Right{} ->
          pure ()

-- | True when the manifest declares no external dependencies.
noDependencies :: Maybe (Map Text a) -> Bool
noDependencies = maybe True Map.null

{- | Namespaced names (e.g. @CoalMicroTest.MicroTest@) of every installed package
module whose unqualified name matches the given module name.
-}
packageProvidersOf :: [(FilePath, Text, [Name])] -> Name -> [Name]
packageProvidersOf namespaces modName =
  nub [ns <> "." <> modName | (_, ns, mods) <- namespaces, modName `elem` mods]

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

{- | Combine the project's own @native_libraries@ with those contributed by
installed packages (including transitive dependencies). Project-declared
libraries come first; duplicates are removed. Order is preserved so that the
resulting 'configNativeLibraries' — and therefore the build-cache hash — is
deterministic.
-}
mergeNativeLibraries :: Maybe [FilePath] -> [FilePath] -> [FilePath]
mergeNativeLibraries projectLibs pkgLibs = nub (fromMaybe [] projectLibs <> pkgLibs)
