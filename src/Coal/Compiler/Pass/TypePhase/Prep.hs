{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.TypePhase.Prep (passPrep) where

import Coal.AST.Metadata (Metadata (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build (Build (..), emptyBuild, insertHash)
import qualified Coal.Compiler.Build as Build
import Coal.Compiler.Build.NameEntry
import Coal.Compiler.Builtin.Definitions (builtinFunctions)
import Coal.Compiler.Builtin.Names (builtinNames)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Compiler.State
import Coal.Language (Kind, constructors)
import Coal.Language.Definition (AliasDefinition (..), Definition (..))
import Coal.Language.Module (Module (..), ModuleExportList (..))
import Coal.Language.Module.Import (Import (..))
import Coal.Language.Module.Path (Path (..), principalPath)
import Coal.Language.Type.Kind.Indexed (ToKindIndexed (..))
import Control.Monad.Except (MonadIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, execStateT, get, gets, modify)
import Control.Monad.Trans (lift)
import Extras (Name, forM, forM_, traverse_)

passPrep :: (MonadIO m) => Pass Metadata m (Module Metadata () ()) (Module Metadata Kind ())
passPrep = Pass{runPass = passImpl}

passImpl :: (MonadIO m) => Module Metadata () () -> CompilerT Metadata m (Module Metadata Kind ())
passImpl m = do
  setCurrentModuleC m
  prep m

prep :: (MonadIO m) => Module Metadata () () -> CompilerT Metadata m (Module Metadata Kind ())
prep m = do
  m1 <- do
    clearAssumptionsC
    clearNameStoreC
    setCurrentModuleC m
    forM_ builtinFunctions $ uncurry insertNameC
    toKindIndexed m

  prepareBuildAliases m1
  insertBuildHash
  return m1

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

prepareDefinitions :: (Monad m) => [Definition a Kind ()] -> ReaderT (ModuleExportList a) (StateT (Build a) (CompilerT a m)) ()
prepareDefinitions = traverse_ collectTypeAliases

-- TODO: DRY
insertExportedName :: (Monad m) => Name -> ReaderT (ModuleExportList a) (StateT (Build a) m) ()
insertExportedName name
  | name `elem` builtinNames =
      pure ()
  | otherwise = do
      exportList <- ask
      case exportList of
        ExportAll ->
          insertName
        Exports exports
          | name `elem` (nameOf <$> exports) ->
              insertName
        _ ->
          pure ()
 where
  insertName = modify (Build.insertBuildExportedName name)

insertNameEntry :: (Monad m) => NameEntry -> ReaderT (ModuleExportList a) (StateT (Build a) m) ()
insertNameEntry entry = modify (Build.insertBuildNameEntry entry)

insertAlias :: (Monad m) => Name -> AliasEntry a -> ReaderT (ModuleExportList a) (StateT (Build a) m) ()
insertAlias name entry = modify (Build.insertBuildAlias name entry)

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
    DImport _ (Path ["Builtin$"]) imports -> do
      pure ()
    DImport _ path imports -> do
      Build{..} <- lift $ lift $ importedBuild path
      forM_ imports $
        \case
          TypeImport loc name _
            | name `elem` buildExportedNames -> do
                build <- lift $ lift $ importedBuild path
                found <- insertTypeName build loc name
                pure ()
            -- unless found $ do
            --  error "TODO"
            -- throwError PreflightFailure

            -- found <- insertTypeName path loc name
            -- unless found $ do
            --  tellErrors [MissingType name path (ErrorLocation this loc)]
            --  throwError PreflightFailure

            | otherwise ->
                pure () -- error (show name)
          _ ->
            pure ()
    DNamespaceImport loc path ->
      pure ()
    _ ->
      pure ()

insertTypeName :: (Monad m) => Build a -> a -> Name -> ReaderT (ModuleExportList a) (StateT (Build a) (CompilerT a m)) Bool
insertTypeName Build{..} loc name =
  or <$> forM (Environment.lookupWithDefault [] name buildNames) go
 where
  go =
    \case
      NTypeAlias{} ->
        case Environment.lookup name buildAliases of
          Nothing ->
            error "TODO"
          Just
            AliasEntry
              { aliasEntryMetadata
              , aliasEntryName
              , aliasEntryParams
              , aliasEntryType
              } -> do
              insertAlias name AliasEntry{..}
              forM_ (constructors aliasEntryType) (insertTypeName Build{..} loc)
              return True
      _ ->
        return False

importedBuild :: (Monad m) => Path -> CompilerT a m (Build a)
importedBuild path = do
  env <- gets compilerModules
  case Environment.lookup (principalPath path) env of
    Nothing ->
      error (show path) -- "TODO"
    Just build ->
      return build
