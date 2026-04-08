{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.TypePhase.Prep (passPrep) where

import Coal.AST.Metadata (Metadata (..))
import qualified Coal.Common.Environment as Environment

-- import Coal.Compiler.Build.Core (buildEnv)
import Coal.Compiler.Builtin.Definitions (builtinFunctions)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Pass.TypePhase.ExpandFunctionGroups
import Coal.Compiler.Stack
import Coal.Language (Kind, constructors)
import Coal.Language.Module (Module (..), fromProtoModule, principalPath, toProtoModule)
import Coal.Language.Module.Export (Export (..), includesName)
import Coal.Language.Module.Import (Import (..))
import Coal.Language.Module.Path (Path (..), principalPath)
import Coal.Language.Type.Kind.Indexed (ToKindIndexed (..))
import Coal.ProtoCompiler.ProtoBuild
import qualified Coal.ProtoCompiler.ProtoBuild as Build
import Coal.ProtoCompiler.ProtoBuild.ProtoNameEntry
import Coal.ProtoCompiler.ProtoBuild.ProtoPrep
import Coal.ProtoCompiler.ProtoStack (ProtoCompilerT (..), insertBuildC, protoOclearAssumptionsC, protoOclearNameStoreC, protoOgetCurrentBuildC, protoOinsertConstraintsC, protoOinsertNameC, protoOupdateSupplyC, setCurrentModuleC, setCurrentPathC)
import Coal.ProtoCompiler.ProtoState
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule (ModuleExportList (..), ProtoModule (..))
import Control.Monad (unless)
import Control.Monad.Except (MonadIO)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.State (StateT, execStateT, get, gets, modify)
import Control.Monad.Trans (lift)
import Data.Set (Set)
import qualified Data.Set as Set
import Extras (Name, for, forM, forM_, second, traverse_, (<.>))

passPrep :: (MonadIO m) => Pass Metadata m (ProtoModule Metadata () ()) (ProtoModule Metadata Kind ())
passPrep = Pass{runPass = pass}

pass :: (MonadIO m) => ProtoModule Metadata () () -> CompilerT Metadata (ProtoCompilerT m Metadata) (ProtoModule Metadata Kind ())
pass m = do
  setCompilerCurrentModuleC (protoOmodulePath m)
  lift $ setCurrentPathC (protoOmodulePath m)
  prep m

-- withCurrentModuleC prep

prep :: (MonadIO m) => ProtoModule Metadata () () -> CompilerT Metadata (ProtoCompilerT m Metadata) (ProtoModule Metadata Kind ())
prep modul = do
  m1 <- lift $ do
    -- let modul = toProtoModule [] m
    protoOclearAssumptionsC
    protoOclearNameStoreC
    setCurrentModuleC modul
    forM_ builtinFunctions $ uncurry protoOinsertNameC
    toKindIndexed modul

  lift $ protoOprepareBuildAliases m1

  expandFunctionGroups m1

protoOprepareBuildAliases ProtoModule{..} = do
  build <-
    execStateT
      (runReaderT (protoOprepareDefinitions protoOmoduleDefinitions) protoOmoduleExportList)
      protoOemptyBuild
        { protoObuildPath = protoOmodulePath
        }
  insertBuildC build

protoOprepareDefinitions :: (Monad m, Monoid a) => [ProtoDefinition a Kind ()] -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) ()
protoOprepareDefinitions = traverse_ collectTypeAliases

-- TODO: DRY
insertExportedName :: (Monad m) => Name -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) m) ()
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

insertNameEntry :: (Monad m) => ProtoNameEntry -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) m) ()
insertNameEntry entry = modify (Build.insertBuildNameEntry entry)

insertAlias :: (Monad m) => Name -> ProtoAliasEntry a -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) m) ()
insertAlias name entry = modify (Build.insertBuildAlias name entry)

collectTypeAliases :: (Monad m) => ProtoDefinition a Kind () -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) ()
collectTypeAliases =
  \case
    ProtoDTypeAlias loc name ProtoAliasDefinition{..} -> do
      insertNameEntry (ProtoNTypeAlias name)
      insertExportedName name
      insertAlias name entry
     where
      entry =
        ProtoAliasEntry
          { protoOaliasEntryMetadata = loc
          , protoOaliasEntryName = name
          , protoOaliasEntryParams = protoOaliasDefinitionParameters
          , protoOaliasEntryType = protoOaliasDefinitionType
          }
    ProtoDImport _ (Path ["Builtin$"]) imports -> do
      pure ()
    ProtoDImport _ path imports -> do
      ProtoBuild{..} <- lift $ lift $ importedBuild path
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
    ProtoDNamespaceImport loc path ->
      pure ()
    _ ->
      pure ()

insertTypeName :: (Monad m) => ProtoBuild a -> a -> Name -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) Bool
insertTypeName ProtoBuild{..} loc name =
  or <$> forM (Environment.lookupWithDefault [] name protoObuildNames) go
 where
  go =
    \case
      ProtoNTypeAlias{} ->
        case Environment.lookup name protoObuildAliases of
          Nothing ->
            error "TODO"
          Just ProtoAliasEntry{..} -> do
            insertAlias name ProtoAliasEntry{..}
            forM_ (constructors protoOaliasEntryType) (insertTypeName ProtoBuild{..} loc)
            return True
      _ ->
        return False

importedBuild :: (Monad m) => Path -> ProtoCompilerT m a (ProtoBuild a)
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
