{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Coal.Compiler.Pass.PhaseTypeChecking.KindIndexing

The kind indexing pass is the entry point to the type checking phase.
It transforms a module from having no kind annotations to having proper
kind annotations on all type parameters, which is a prerequisite for
subsequent type inference.

This pass performs three main tasks:

1. Kind indexing: Converts the module to kind-indexed form via 'toKindIndexed',
   assigning proper 'Kind' annotations to type parameters throughout the AST.

2. Environment setup: Initializes the typing environment by clearing previous
   state and inserting builtin functions into the name store.

3. Build preparation: Collects type aliases and their metadata from the module
   and its imports, preparing the Build structure for later compilation phases.

The pass operates on 'Module Metadata () ()' (no kind or type information)
and produces 'Module Metadata Kind ()' (with kind annotations but no types yet).
-}
module Coal.Compiler.Pass.PhaseTypeChecking.KindIndexing (passKindIndexing) where

import Coal.AST.Metadata (Metadata (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build (Build (..), emptyBuild, insertHash)
import qualified Coal.Compiler.Build as Build
import Coal.Compiler.Build.NameEntry
import Coal.Compiler.Builtin.Definitions (builtinFunctions)
import Coal.Compiler.Builtin.Names (builtinNames)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT, clearAssumptionsC, clearNameStoreC, insertBuildC, insertNameC, setCurrentModuleC, updateCurrentBuildPureC)
import Coal.Compiler.State
import Coal.Language (Kind, constructors)
import Coal.Language.Definition (AliasDefinition (..), Definition (..))
import Coal.Language.Module (ExportList (..), Module (..))
import Coal.Language.Module.Import (Import (..))
import Coal.Language.Module.Path (Path (..), principalPath)
import Coal.Language.Type.Kind.Indexed (ToKindIndexed (..))
import Control.Monad (when)
import Control.Monad.Except (MonadIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, execStateT, get, gets, modify)
import Control.Monad.Trans (lift)
import Extras (Name, forM, forM_, traverse_)

{- | The kind indexing compiler pass.

Transforms a module from 'Module Metadata () ()' to 'Module Metadata Kind ()',
annotating all type parameters with their kinds and preparing the build
environment for subsequent type checking passes.

This is the first pass in the type phase pipeline and must complete successfully
before any type inference can occur.
-}
passKindIndexing :: (MonadIO m) => Pass Metadata m (Module Metadata () ()) (Module Metadata Kind ())
passKindIndexing = Pass{runPass = passImpl}

passImpl :: (MonadIO m) => Module Metadata () () -> CompilerT Metadata m (Module Metadata Kind ())
passImpl m = do
  setCurrentModuleC m
  kindIndexing m

{- This function:

1. Clears previous assumptions and name stores to ensure a clean state
2. Inserts builtin functions into the name environment
3. Transforms the module to kind-indexed form via 'toKindIndexed'
4. Prepares build aliases by collecting type alias information
5. Inserts a hash of the source code for change detection

The resulting module has all type parameters annotated with their kinds.
-}
kindIndexing :: (MonadIO m) => Module Metadata () () -> CompilerT Metadata m (Module Metadata Kind ())
kindIndexing m = do
  indexedM <- do
    clearAssumptionsC
    clearNameStoreC
    setCurrentModuleC m
    forM_ builtinFunctions (uncurry insertNameC)
    toKindIndexed m

  prepareBuildAliases indexedM
  insertBuildHash
  return indexedM

prepareBuildAliases :: (Monad m) => Module a Kind () -> CompilerT a m ()
prepareBuildAliases Module{..} = do
  build <-
    execStateT
      (runReaderT (prepareDefinitions moduleDefinitions) moduleExportList)
      emptyBuild
        { buildPath = modulePath
        }
  insertBuildC build

insertBuildHash :: (Monad m) => CompilerT a m ()
insertBuildHash = do
  CompilerState{..} <- get
  case Environment.lookup (principalPath compilerCurrentPath) compilerSources of
    Nothing ->
      pure ()
    Just source ->
      updateCurrentBuildPureC (insertHash source)

prepareDefinitions :: (Monad m) => [Definition a Kind ()] -> ReaderT (ExportList a) (StateT (Build a) (CompilerT a m)) ()
prepareDefinitions = traverse_ collectTypeAliases

insertExportedName :: (Monad m) => Name -> ReaderT (ExportList a) (StateT (Build a) m) ()
insertExportedName name
  | name `elem` builtinNames =
      pure ()
  | otherwise = do
      exportList <- ask
      when (isExported name exportList) insertName
 where
  insertName = modify (Build.insertBuildExportedName name)

  isExported :: Name -> ExportList a -> Bool
  isExported _ ExportAll = True
  isExported n (Exports exports) = n `elem` (nameOf <$> exports)

insertNameEntry :: (Monad m) => NameEntry -> ReaderT (ExportList a) (StateT (Build a) m) ()
insertNameEntry entry = modify (Build.insertBuildNameEntry entry)

insertAlias :: (Monad m) => Name -> AliasEntry a -> ReaderT (ExportList a) (StateT (Build a) m) ()
insertAlias name entry = modify (Build.insertBuildAlias name entry)

collectTypeAliases :: (Monad m) => Definition a Kind () -> ReaderT (ExportList a) (StateT (Build a) (CompilerT a m)) ()
collectTypeAliases =
  \case
    DTypeAlias loc name AliasDefinition{..} -> do
      insertNameEntry (NTypeAlias name)
      insertExportedName name
      insertAlias name entry
     where
      entry =
        AliasEntry
          { aliasEntryMetadata = loc
          , aliasEntryName = name
          , aliasEntryParams = aliasDefinitionParameters
          , aliasEntryType = aliasDefinitionType
          }
    -- Skip built-in imports as they are handled separately
    DImport _ (Path ["Builtin$"]) _ ->
      pure ()
    DImport _ path imports -> do
      Build{..} <- lift $ lift $ importedBuild path
      forM_ imports $
        \case
          TypeImport loc name _
            | name `elem` buildExportedNames -> do
                build <- lift $ lift $ importedBuild path
                _ <- insertTypeName build loc name
                pure ()
            | otherwise ->
                -- Type not found in exported names; silently skip
                -- (error reporting happens in later phases)
                pure ()
          -- Non-type imports (value imports, etc.) are handled in other passes
          _ ->
            pure ()
    DNamespaceImport _ _ ->
      pure ()
    _ ->
      pure ()

insertTypeName :: (Monad m) => Build a -> a -> Name -> ReaderT (ExportList a) (StateT (Build a) (CompilerT a m)) Bool
insertTypeName Build{..} loc name =
  or <$> forM (Environment.lookupWithDefault [] name buildNames) go
 where
  go =
    \case
      NTypeAlias{} ->
        case Environment.lookup name buildAliases of
          Nothing ->
            -- Invariant violation: name is in buildNames but not in buildAliases
            -- This should not happen; indicates a compiler bug
            error $ "Internal error: Type alias " ++ show name ++ " found in buildNames but not in buildAliases"
          Just
            AliasEntry
              { aliasEntryMetadata
              , aliasEntryName
              , aliasEntryParams
              , aliasEntryType
              } -> do
              insertAlias name AliasEntry{..}
              -- Recursively insert constructors used in the alias type
              forM_ (constructors aliasEntryType) (insertTypeName Build{..} loc)
              return True
      _ ->
        return False

importedBuild :: (Monad m) => Path -> CompilerT a m (Build a)
importedBuild path = do
  env <- gets compilerModules
  case Environment.lookup (principalPath path) env of
    Nothing ->
      -- Module not found in compiler state; this indicates the module
      -- was not processed or there is a dependency resolution issue
      error $ "Internal error: Module " ++ show path ++ " not found in compiler modules. Check dependency resolution."
    Just build ->
      return build
