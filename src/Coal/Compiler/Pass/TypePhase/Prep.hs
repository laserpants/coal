{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.TypePhase.Prep (passPrep) where

import Coal.AST.Metadata (Metadata (..))
import qualified Coal.Common.Environment as Environment
import Debug.Trace
import Coal.Compiler.Build
import qualified Coal.Compiler.Build as Build
import Coal.Compiler.Build.NameEntry
import Coal.Compiler.Build.Prep
import Coal.Compiler.Builtin.Definitions (builtinFunctions)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Pass.TypePhase.ExpandFunctionGroups
import Coal.Compiler.Stack
import Coal.Compiler.State
import Coal.Language (Kind, constructors)
import Coal.Language.Definition
import Coal.Language.Module (Module (..), ModuleExportList (..))
import Coal.Language.Module.Export (Export (..), includesName)
import Coal.Language.Module.Import (Import (..))
import Coal.Language.Module.Path (Path (..), principalPath)
import Coal.Language.Type.Kind.Indexed (ToKindIndexed (..))
import Control.Monad (unless)
import Control.Monad.Except (MonadIO)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.State (StateT, execStateT, get, gets, modify)
import Control.Monad.Trans (lift)
import Data.Set (Set)
import qualified Data.Set as Set
import Extras (Name, for, forM, forM_, second, traverse_, (<.>))

passPrep :: (MonadIO m) => Pass Metadata m (Module Metadata () ()) (Module Metadata Kind ())
passPrep = Pass{runPass = pass}

pass :: (MonadIO m) => Module Metadata () () -> CompilerT Metadata m (Module Metadata Kind ())
pass m = do
  -- setCompilerCurrentModuleC (modulePath m)
  setCurrentPathC (modulePath m)
  prep m

-- withCurrentModuleC prep

prep :: (MonadIO m) => Module Metadata () () -> CompilerT Metadata m (Module Metadata Kind ())
prep modul = do
  m1 <- do
    -- let modul = toModule [] m
    clearAssumptionsC
    clearNameStoreC
    setCurrentModuleC modul -- ??
    forM_ builtinFunctions $ uncurry insertNameC
    toKindIndexed modul

  prepareBuildAliases m1

  hashBuild

  expandFunctionGroups m1

prepareBuildAliases Module{..} = do
  build <-
    execStateT
      (runReaderT (prepareDefinitions moduleDefinitions) moduleExportList)
      emptyBuild
        { buildPath = modulePath
        }
  insertBuildC build

hashBuild :: (Monad m) => CompilerT a m ()
hashBuild = do
  Build{..} <- getCurrentBuildC
  env <- gets compilerSources

  case Environment.lookup (principalPath buildPath) env of
    Nothing -> do
      pure ()
    Just source -> do

      -- TODO: modifyBuild ??
      newBuild <-
        flip execStateT Build{..} $ do
          modify (insertHash source)
      insertBuildC newBuild

prepareDefinitions :: (Monad m, Monoid a) => [Definition a Kind ()] -> ReaderT (ModuleExportList a) (StateT (Build a) (CompilerT a m)) ()
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

builtinNames :: Set Name
builtinNames =
  Set.fromList
    [ "(%)"
    , "(*)"
    , "(+)"
    , "(-)"
    , "(/)"
    , "(<>)"
    , "(==)"
    , "(!=)"
    , "Comparable"
    , "Divisible"
    , "EqualTo"
    , "GreaterThan"
    , "IO"
    , "LessThan"
    , "Modulo"
    , "None"
    , "Numeric"
    , "Option"
    , "Result"
    , "Ok"
    , "Err"
    , "Ordered"
    , "Ordering"
    , "Semigroup"
    , "Some"
    , "Process"
    , "compare"
    , "from_int32"
    , "from_int64"
    , "from_bignum"
    , "negate"
    ]

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
          Just AliasEntry{..} -> do
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

--  y <- expandFunctionGroups m1
--  lift $ prepareBuild y
--
--  --  clearAssumptionsC
--  --  clearNameStoreC
--  --  (next, build) <- prepareBuild m
--  --  insertCurrentModuleC build
--  --  env <- buildEnv
--  --
--  --  setNamesC env
--  --  insertNamesC builtinFunctions
--
--  pure y
