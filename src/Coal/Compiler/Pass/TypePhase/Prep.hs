{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.TypePhase.Prep (passPrep) where

import Coal.AST.Metadata (Metadata (..))
import qualified Coal.Common.Environment as Environment

-- import Coal.Compiler.Build.Core (buildEnv)

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
  -- setCompilerCurrentModuleC (protoOmodulePath m)
  setCurrentPathC (protoOmodulePath m)
  prep m

-- withCurrentModuleC prep

prep :: (MonadIO m) => Module Metadata () () -> CompilerT Metadata m (Module Metadata Kind ())
prep modul = do
  m1 <- do
    -- let modul = toModule [] m
    protoOclearAssumptionsC
    protoOclearNameStoreC
    setCurrentModuleC modul -- ??
    forM_ builtinFunctions $ uncurry protoOinsertNameC
    toKindIndexed modul

  protoOprepareBuildAliases m1

  expandFunctionGroups m1

protoOprepareBuildAliases Module{..} = do
  build <-
    execStateT
      (runReaderT (protoOprepareDefinitions protoOmoduleDefinitions) protoOmoduleExportList)
      protoOemptyBuild
        { protoObuildPath = protoOmodulePath
        }
  insertBuildC build

protoOprepareDefinitions :: (Monad m, Monoid a) => [Definition a Kind ()] -> ReaderT (ModuleExportList a) (StateT (Build a) (CompilerT a m)) ()
protoOprepareDefinitions = traverse_ collectTypeAliases

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
          | name `elem` (protoOnameOf <$> exports) ->
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
          { protoOaliasEntryMetadata = loc
          , protoOaliasEntryName = name
          , protoOaliasEntryParams = protoOaliasDefinitionParameters
          , protoOaliasEntryType = protoOaliasDefinitionType
          }
    DImport _ (Path ["Builtin$"]) imports -> do
      pure ()
    DImport _ path imports -> do
      Build{..} <- lift $ lift $ importedBuild path
      forM_ imports $
        \case
          TypeImport loc name _
            | name `elem` protoObuildExportedNames -> do
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
  or <$> forM (Environment.lookupWithDefault [] name protoObuildNames) go
 where
  go =
    \case
      NTypeAlias{} ->
        case Environment.lookup name protoObuildAliases of
          Nothing ->
            error "TODO"
          Just AliasEntry{..} -> do
            insertAlias name AliasEntry{..}
            forM_ (constructors protoOaliasEntryType) (insertTypeName Build{..} loc)
            return True
      _ ->
        return False

importedBuild :: (Monad m) => Path -> CompilerT a m (Build a)
importedBuild path = do
  env <- gets protoOcompilerModules
  case Environment.lookup (principalPath path) env of
    Nothing ->
      error (show path) -- "TODO"
    Just build ->
      return build

--  y <- expandFunctionGroups m1
--  lift $ protoOprepareBuild y
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
