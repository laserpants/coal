{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PhaseParsing.Parsing (
  passParsing,
  fromSource,
  buildPackageNsMap,
  packageModuleProviders,
  ambiguousPackageModules,
  localAwareNsMap,
) where

import Coal.Compiler.Build.Cache (cachedBuild)
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Builtin.Modules (builtinModules)
import Coal.Compiler.Config
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Path.Resolve (resolveModule)
import Coal.Compiler.Stack
import Coal.Compiler.State (CompilerState (compilerConfig))
import Coal.Language.Definition (Definition (..))
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path (Path (..), principalPath)
import Coal.Parser (ParserError, parseSourceFile)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (gets)
import qualified Data.ByteString as B
import Data.Either (partitionEithers)
import Data.List (find, nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as Text
import Extras (Name, forM_)
import Text.Megaparsec (runParser)

passParsing :: (MonadIO m) => Pass Metadata m [FilePath] [BuildEnvelope (Module Metadata () ())]
passParsing = Pass{runPass = passImpl}

passImpl :: (MonadIO m) => [FilePath] -> CompilerT Metadata m [BuildEnvelope (Module Metadata () ())]
passImpl files = do
  builtinModulesFiles <- traverse parseEmbedded builtinModules
  builtinModulesBundle <- handleParseResults builtinModulesFiles $ \(p, e) ->
    error ("Error in builtinModules module '" <> Text.unpack p <> "': " <> show e)
  results <- traverse parseFile files
  bundle <- handleParseResults results (tellErrors . return)
  pure (builtinModulesBundle <> bundle)

-- | Helper to handle parsing results: report errors or return bundles
handleParseResults :: (MonadIO m) => [Either e a] -> (e -> CompilerT Metadata m ()) -> CompilerT Metadata m [a]
handleParseResults results reportError = do
  case partitionEithers results of
    ([], bundles) ->
      pure bundles
    (errors, _) -> do
      forM_ errors reportError
      throwError ParserFailure

-- | Check cache and handle source registration for a given module name and source
checkCacheAndRegister :: (MonadIO m) => Name -> Text -> Module Metadata () () -> CompilerT Metadata m (BuildEnvelope (Module Metadata () ()))
checkCacheAndRegister name src m = do
  CompilerConfig{configNoCache} <- gets compilerConfig
  cached <- cachedBuild name src
  setBuildSourceC name src
  case cached of
    Just build | not configNoCache -> do
      insertBuildC build
      pure (BCached build)
    _ -> do
      setTouchedC name
      pure (BSource m)

parseEmbedded :: (MonadIO m) => (Text, B.ByteString) -> CompilerT Metadata m (Either (Text, ParserError) (BuildEnvelope (Module Metadata () ())))
parseEmbedded (p, src) = do
  let encodedSrc = E.decodeUtf8 src
  case runParser parseSourceFile "" encodedSrc of
    Left err ->
      pure $ Left (p, err)
    Right m -> do
      let name = principalPath (modulePath m)
      Right <$> checkCacheAndRegister name encodedSrc m

{- | Parse and register a source file that belongs to the project itself.

Imports are rewritten using @nsMap@, which maps short package module names to
their namespaced form. Locally-defined modules take precedence over same-named
package modules: @nsMap@ is expected to already have been filtered by
'localAwareNsMap'. An import whose short name is provided by more than one
package (and is not shadowed by a local module) is rejected as ambiguous via
@ambiguous@.
-}
fromSource :: (MonadIO m) => Map.Map Name Name -> Map.Map Name [Name] -> Set Name -> Name -> FilePath -> Text -> CompilerT Metadata m (Either (CompilerError Metadata) (BuildEnvelope (Module Metadata () ())))
fromSource nsMap ambiguous localModules name file src = do
  case runParser parseSourceFile "" src of
    Left err ->
      pure $ Left (ParserError file err)
    Right (Module path exportList defs) ->
      if principalPath path == name
        then case firstAmbiguousImport ambiguous localModules defs of
          Just (imported, providers) ->
            pure $ Left (AmbiguousModuleImport file imported providers)
          Nothing -> do
            let m' = Module path exportList (rewriteImports nsMap defs)
            Right <$> checkCacheAndRegister name src m'
        else pure $ Left (BadModuleName file (principalPath path))

parseFile :: (MonadIO m) => FilePath -> CompilerT Metadata m (Either (CompilerError Metadata) (BuildEnvelope (Module Metadata () ())))
parseFile file = do
  CompilerConfig{..} <- gets compilerConfig
  let nsMap = buildPackageNsMap configPackageNamespaces
      -- Locally-defined modules shadow same-named package modules, so the map
      -- used for the project's own modules drops those short names.
      localNsMap = localAwareNsMap configLocalModules nsMap
      ambiguous = ambiguousPackageModules configPackageNamespaces
      localModules = Set.fromList configLocalModules
      -- Package source roots are used only to classify package files (which are
      -- absolute). They are not searched for the project's own relative module
      -- names, so a module listed in the manifest but missing from the project
      -- can never silently resolve to a same-named file inside a dependency.
      packageDirs = [dir | (dir, _, _) <- configPackageNamespaces]
  res <- liftIO $ resolveModule configSourcePaths (configSourcePaths <> packageDirs) file
  case res of
    Right (fp, bestRoot, derivedName) -> do
      src <- liftIO (Text.readFile fp)
      case lookupPackageNamespace bestRoot configPackageNamespaces of
        Nothing ->
          fromSource localNsMap ambiguous localModules derivedName file src
        Just (ns, _modSet) ->
          fromSourceNamespaced ns nsMap derivedName file src
    Left err ->
      pure $ Left (BadFilename file err)

-- | Look up the package namespace for a given canonical source root.
lookupPackageNamespace :: FilePath -> [(FilePath, Text, [Name])] -> Maybe (Text, [Name])
lookupPackageNamespace bestRoot namespaces =
  (\(_, ns, mods) -> (ns, mods)) <$> find (\(dir, _, _) -> dir == bestRoot) namespaces

{- | Map from an unqualified module name to every fully-namespaced name that
provides it, across all installed packages. The list preserves the order of the
supplied namespaces (which is deterministic) and has duplicates removed.
-}
packageModuleProviders :: [(FilePath, Text, [Name])] -> Map.Map Name [Name]
packageModuleProviders namespaces =
  Map.map nub $
    Map.fromListWith
      (flip (<>))
      [ (modName, [ns <> "." <> modName])
      | (_, ns, mods) <- namespaces
      , modName <- mods
      ]

{- | Short module names that are provided by more than one installed package,
mapped to every namespaced name that provides them. Importing such a name
without a local module to disambiguate it is reported as an error.
-}
ambiguousPackageModules :: [(FilePath, Text, [Name])] -> Map.Map Name [Name]
ambiguousPackageModules = Map.filter ((> 1) . length) . packageModuleProviders

{- | Build a map from unqualified module name to its fully-namespaced name
covering every module from every installed package. When several packages
provide the same short name the first namespace in the supplied order wins;
such names are also reported by 'ambiguousPackageModules'.
-}
buildPackageNsMap :: [(FilePath, Text, [Name])] -> Map.Map Name Name
buildPackageNsMap = Map.mapMaybe listToMaybe . packageModuleProviders

{- | Restrict a package namespace map to the short names that are /not/ defined
by the project itself, so that locally-defined modules shadow same-named package
modules during import rewriting.
-}
localAwareNsMap :: [Name] -> Map.Map Name Name -> Map.Map Name Name
localAwareNsMap localModules nsMap =
  let local = Set.fromList localModules
   in Map.filterWithKey (\k _ -> not (Set.member k local)) nsMap

{- | Find the first import of a locally-defined module whose short module name
is provided by more than one installed package. With no local module of the
same name to shadow it, the compiler cannot tell which package module is meant.
-}
firstAmbiguousImport :: Map.Map Name [Name] -> Set Name -> [Definition Metadata () ()] -> Maybe (Name, [Name])
firstAmbiguousImport ambiguous localModules = listToMaybe . mapMaybe check
 where
  check (DImport _ path _) = ambiguousImport path
  check (DNamespaceImport _ path) = ambiguousImport path
  check _ = Nothing
  ambiguousImport path =
    let short = principalPath path
     in if Set.member short localModules
          then Nothing
          else (\providers -> (short, providers)) <$> Map.lookup short ambiguous

{- | Parse a source file that belongs to an installed package, injecting
the package namespace into the module path and rewriting all imports that
refer to any known package module (intra- or cross-package).

The file may declare @module Spec@ while the derived name is @\"Spec\"@; after
successful validation the module is renamed to @Foo.Spec@ and all imports of
package modules are updated to their namespaced form.
-}
fromSourceNamespaced ::
  (MonadIO m) =>
  -- | Namespace prefix for this package, e.g. @\"Foo\"@
  Text ->
  -- | Full map: unqualified module name → namespaced name (all packages)
  Map.Map Name Name ->
  -- | Module name derived from the file path (unqualified), e.g. @\"Spec\"@
  Name ->
  FilePath ->
  Text ->
  CompilerT Metadata m (Either (CompilerError Metadata) (BuildEnvelope (Module Metadata () ())))
fromSourceNamespaced ns nsMap derivedName file src = do
  case runParser parseSourceFile "" src of
    Left err ->
      pure $ Left (ParserError file err)
    Right (Module path exportList defs) ->
      if principalPath path /= derivedName
        then pure $ Left (BadModuleName file (principalPath path))
        else do
          let namespacedName = ns <> "." <> derivedName
              nsComps = Text.splitOn "." ns
              rewrittenPath = Path (nsComps <> pathComponents path)
              rewrittenDefs = rewriteImports nsMap defs
              m' = Module rewrittenPath exportList rewrittenDefs
          Right <$> checkCacheAndRegister namespacedName src m'

{- | Rewrite import paths that refer to any known package module so that
they use the fully-namespaced form.  Non-package imports are left unchanged.

Example: with @nsMap = [(\"Bar\", \"Foo.Bar\"), (\"MicroTest\", \"CoalMicroTest.MicroTest\")]@,
@import Bar (...)@ becomes @import Foo.Bar (...)@ and
@import MicroTest (...)@ becomes @import CoalMicroTest.MicroTest (...)@.
-}
rewriteImports :: Map.Map Name Name -> [Definition Metadata () ()] -> [Definition Metadata () ()]
rewriteImports nsMap = fmap go
 where
  go (DImport loc path imports)
    | Just newName <- Map.lookup (principalPath path) nsMap =
        DImport loc (namespacedPath newName) imports
  go (DNamespaceImport loc path)
    | Just newName <- Map.lookup (principalPath path) nsMap =
        DNamespaceImport loc (namespacedPath newName)
  go d = d

  namespacedPath n = Path (Text.splitOn "." n)
