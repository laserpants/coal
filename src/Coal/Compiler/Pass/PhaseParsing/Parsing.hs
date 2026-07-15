{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PhaseParsing.Parsing (passParsing, fromSource) where

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
import Data.List (find)
import qualified Data.Map.Strict as Map
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

fromSource :: (MonadIO m) => Map.Map Name Name -> Name -> FilePath -> Text -> CompilerT Metadata m (Either (CompilerError Metadata) (BuildEnvelope (Module Metadata () ())))
fromSource nsMap name file src = do
  case runParser parseSourceFile "" src of
    Left err ->
      pure $ Left (ParserError file err)
    Right (Module path exportList defs) ->
      if principalPath path == name
        then do
          let m' = Module path exportList (rewriteImports nsMap defs)
          Right <$> checkCacheAndRegister name src m'
        else pure $ Left (BadModuleName file (principalPath path))

parseFile :: (MonadIO m) => FilePath -> CompilerT Metadata m (Either (CompilerError Metadata) (BuildEnvelope (Module Metadata () ())))
parseFile file = do
  CompilerConfig{..} <- gets compilerConfig
  let nsMap = buildPackageNsMap configPackageNamespaces
  res <- liftIO $ resolveModule configSourcePaths file
  case res of
    Right (fp, bestRoot, derivedName) -> do
      src <- liftIO (Text.readFile fp)
      case lookupPackageNamespace bestRoot configPackageNamespaces of
        Nothing ->
          fromSource nsMap derivedName file src
        Just (ns, _modSet) ->
          fromSourceNamespaced ns nsMap derivedName file src
    Left err ->
      pure $ Left (BadFilename file err)

-- | Look up the package namespace for a given canonical source root.
lookupPackageNamespace :: FilePath -> [(FilePath, Text, [Name])] -> Maybe (Text, [Name])
lookupPackageNamespace bestRoot namespaces =
  (\(_, ns, mods) -> (ns, mods)) <$> find (\(dir, _, _) -> dir == bestRoot) namespaces

{- | Build a map from unqualified module name to fully-namespaced name
covering every module from every installed package.
-}
buildPackageNsMap :: [(FilePath, Text, [Name])] -> Map.Map Name Name
buildPackageNsMap namespaces =
  Map.fromList
    [ (modName, ns <> "." <> modName)
    | (_, ns, mods) <- namespaces
    , modName <- mods
    ]

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
              rewrittenPath = Path (nsComps ++ pathComponents path)
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
rewriteImports nsMap = map go
 where
  go (DImport loc path imports)
    | Just newName <- Map.lookup (principalPath path) nsMap =
        DImport loc (namespacedPath newName) imports
  go (DNamespaceImport loc path)
    | Just newName <- Map.lookup (principalPath path) nsMap =
        DNamespaceImport loc (namespacedPath newName)
  go d = d

  namespacedPath n = Path (Text.splitOn "." n)
