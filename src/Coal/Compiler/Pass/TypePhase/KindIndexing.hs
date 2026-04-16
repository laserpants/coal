{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Coal.Compiler.Pass.TypePhase.KindIndexing

The kind indexing pass is the entry point to the type checking phase.
It transforms a module from having no kind annotations to having proper
kind annotations on all type parameters, which is a prerequisite for
subsequent type inference.

This pass performs three main tasks:

1. Kind Indexing: Converts the module to kind-indexed form via 'toKindIndexed',
   assigning proper 'Kind' annotations to type parameters throughout the AST.

2. Environment Setup: Initializes the typing environment by clearing previous
   state and inserting builtin functions into the name store.

3. Build Preparation: Collects type aliases and their metadata from the module
   and its imports, preparing the Build structure for later compilation phases.

The pass operates on 'Module Metadata () ()' (no kind or type information)
and produces 'Module Metadata Kind ()' (with kind annotations but no types yet).
-}
module Coal.Compiler.Pass.TypePhase.KindIndexing (passKindIndexing) where

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
import Coal.Language.Module (Module (..), ModuleExportList (..))
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

{- | Implementation of the kind indexing pass.

Sets the current module context and delegates to 'kindIndexing'.
-}
passImpl :: (MonadIO m) => Module Metadata () () -> CompilerT Metadata m (Module Metadata Kind ())
passImpl m = do
  setCurrentModuleC m
  kindIndexing m

{- | Perform kind indexing on a module.

This function:

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
    forM_ builtinFunctions $ uncurry insertNameC
    toKindIndexed m

  prepareBuildAliases indexedM
  insertBuildHash
  return indexedM

{- | Prepare build information for type aliases in the module.

Traverses all definitions in the module and collects type alias information
into a 'Build' structure, which is then inserted into the compiler state.
This information is used by later passes to expand type aliases and resolve
type names.

The function uses a 'ReaderT' over 'StateT' transformer stack to thread
the module's export list and accumulate build information.
-}
prepareBuildAliases :: (Monad m) => Module a Kind () -> CompilerT a m ()
prepareBuildAliases Module{..} = do
  build <-
    execStateT
      (runReaderT (prepareDefinitions moduleDefinitions) moduleExportList)
      emptyBuild
        { buildPath = modulePath
        }
  insertBuildC build

{- | Insert a hash of the source code into the current build.

This hash is used for change detection and incremental compilation.
If the module's source is found in the compiler state, its hash is
computed and stored in the build information.
-}
insertBuildHash :: (Monad m) => CompilerT a m ()
insertBuildHash = do
  CompilerState{..} <- get
  case Environment.lookup (principalPath compilerCurrentPath) compilerSources of
    Nothing ->
      pure ()
    Just source ->
      updateCurrentBuildPureC (insertHash source)

{- | Process a list of definitions to collect type information.

Currently focuses on collecting type aliases via 'collectTypeAliases'.
Other definition types are handled in subsequent passes.
-}
prepareDefinitions :: (Monad m) => [Definition a Kind ()] -> ReaderT (ModuleExportList a) (StateT (Build a) (CompilerT a m)) ()
prepareDefinitions = traverse_ collectTypeAliases

{- | Insert a name into the build's exported names list if it should be exported.

Builtin names are never added to the export list as they're handled separately.
For other names, checks the module's export list to determine if the name
should be exported.

The 'isExported' helper function encapsulates the export checking logic:

* 'ExportAll': All names are exported
* 'Exports': Only explicitly listed names are exported
-}
insertExportedName :: (Monad m) => Name -> ReaderT (ModuleExportList a) (StateT (Build a) m) ()
insertExportedName name
  | name `elem` builtinNames =
      pure ()
  | otherwise = do
      exportList <- ask
      when (isExported name exportList) insertName
 where
  insertName = modify (Build.insertBuildExportedName name)

  isExported :: Name -> ModuleExportList a -> Bool
  isExported _ ExportAll = True
  isExported n (Exports exports) = n `elem` (nameOf <$> exports)

{- | Insert a name entry into the build's name registry.

Name entries categorize names by their role (type alias, data constructor, etc.)
and are used during name resolution in later compilation phases.
-}
insertNameEntry :: (Monad m) => NameEntry -> ReaderT (ModuleExportList a) (StateT (Build a) m) ()
insertNameEntry entry = modify (Build.insertBuildNameEntry entry)

{- | Insert a type alias entry into the build's alias registry.

Stores the complete information about a type alias including its parameters
and definition, which will be used during alias expansion in later passes.
-}
insertAlias :: (Monad m) => Name -> AliasEntry a -> ReaderT (ModuleExportList a) (StateT (Build a) m) ()
insertAlias name entry = modify (Build.insertBuildAlias name entry)

{- | Collect type alias information from a definition.

Handles different definition types:

* 'DTypeAlias': Records the alias name, parameters, and type in the build
* 'DImport': Processes imported type aliases from dependencies
* Other definitions: Ignored in this pass (handled elsewhere)

For imports, recursively includes transitive dependencies by processing
constructors used in imported type aliases.
-}
collectTypeAliases :: (Monad m) => Definition a Kind () -> ReaderT (ModuleExportList a) (StateT (Build a) (CompilerT a m)) ()
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
    -- Skip builtin imports as they are handled separately
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

{- | Insert a type name and its transitive dependencies from an imported module.

Looks up the name in the provided build's name registry and, if it's a type
alias, inserts it along with all constructors used in the alias type.
This ensures that all transitive type dependencies are available.

Returns 'True' if the name was found and inserted, 'False' otherwise.

Note: Throws an internal error if a name is found in 'buildNames' but not
in 'buildAliases', which would indicate a compiler bug.
-}
insertTypeName :: (Monad m) => Build a -> a -> Name -> ReaderT (ModuleExportList a) (StateT (Build a) (CompilerT a m)) Bool
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

{- | Retrieve the build information for an imported module.

Looks up the module's build in the compiler state. The build contains
all the type and name information that has been processed for that module.

Throws an internal error if the module is not found, which indicates either:

* The module was not processed yet (dependency resolution issue)
* The module path is incorrect
* There's a bug in the compilation pipeline
-}
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
