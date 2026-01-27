{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoCompiler.ProtoBuild.ProtoPrep (protoOprepareBuild) where

import qualified Coal.Common.Environment as Environment
import Coal.Language
import Coal.Language.Module.Export (Export (..), includesName)
import Coal.ProtoCompiler.ProtoBuild
import qualified Coal.ProtoCompiler.ProtoBuild as Build
import Coal.ProtoCompiler.ProtoBuild.ProtoNameEntry
import Coal.ProtoCompiler.ProtoStack (ProtoCompilerT (..))
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule (ModuleExportList (..), ProtoModule (..))
import Coal.ProtoTypeSystem.Parameterized (ProtoParameterized (..), ToIndexed (..))
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.State (StateT, execStateT, get, modify)
import Control.Monad.Trans (lift)
import Data.Set (Set)
import qualified Data.Set as Set
import Extras (Name, for, forM, forM_, traverse_)

insertNameEntry :: (Monad m) => Name -> ProtoNameEntry -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) m) ()
insertNameEntry name entry = modify (Build.insertBuildNameEntry name entry)

insertDataConstructor :: (Monad m) => Name -> ProtoDataConstructorEntry a -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) m) ()
insertDataConstructor name entry = modify (Build.insertBuildDataConstructor name entry)

insertTypeConstructor :: (Monad m) => Name -> ProtoTypeConstructorEntry a -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) m) ()
insertTypeConstructor name entry = modify (Build.insertBuildTypeConstructor name entry)

insertTrait :: (Monad m) => Name -> ProtoTraitEntry a -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) m) ()
insertTrait name entry = modify (Build.insertBuildTrait name entry)

insertInstance :: (Monad m) => Name -> IndexedType -> ProtoInstanceEntry a -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) m) ()
insertInstance name t entry = modify (Build.insertBuildInstance name t entry)

insertExportedName :: (Monad m) => Name -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) m) ()
insertExportedName name = do
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

protoOprepareBuild :: (Monad m) => ProtoModule a Kind () -> ProtoCompilerT m a (ProtoBuild a)
protoOprepareBuild ProtoModule{..} =
  execStateT
    (runReaderT (protoOprepareDefinitions protoOmoduleDefinitions) protoOmoduleExportList)
    protoOemptyBuild
      { protoObuildPath = protoOmodulePath
      }

protoOprepareDefinitions :: (Monad m) => [ProtoDefinition a Kind ()] -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) ()
protoOprepareDefinitions defs = do
  -- collect type constructors
  traverse_ collectTypeConstructors defs
  -- collect data constructors
  traverse_ collectDataConstructors defs
  -- expand exports
  exports <- expandExports
  local (const exports) $ do
    -- collect traits
    traverse_ collectTraits defs
    -- collect trait interfaces
    traverse_ collectTraitsInterface defs
    -- collect instances
    traverse_ collectInstances defs
    -- collect placeholders
    traverse_ collectPlaceholders defs

expandExports :: (Monad m) => ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) (ModuleExportList a)
expandExports = do
  exportList <- ask
  ProtoBuild{protoObuildDataConstructors} <- get
  case exportList of
    ExportAll ->
      return ExportAll
    Exports exports -> do
      newExports <-
        forM exports $
          \case
            TypeExport loc name [] ->
              case Environment.lookup name protoObuildDataConstructors of
                Nothing ->
                  error "TODO"
                Just ProtoDataConstructorEntry{..} ->
                  return (TypeExport loc name (Set.toList protoOdataConstructorEntryConstructorSet))
            e ->
              return e
      return
        (Exports newExports)

collectTypeConstructors :: (Monad m) => ProtoDefinition a Kind () -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) ()
collectTypeConstructors =
  \case
    ProtoDType loc name ProtoTypeDefinition{..} -> do
      insertNameEntry name (ProtoNType name kind)
      insertExportedName name
      insertTypeConstructor name entry
     where
      kind = foldKindOf KType protoOtypeDefinitionParameters
      entry =
        ProtoTypeConstructorEntry
          { protoOtypeConstructorEntryMetadata = loc
          , protoOtypeConstructorEntryName = name
          , protoOtypeConstructorEntryKind = kind
          , protoOtypeConstructorEntryDataConstructors =
              for protoOtypeDefinitionConstructors constructorName
          }
    -- TODO
    ProtoDImport loc path items ->
      pure ()
    -- TODO
    ProtoDQualifiedImport loc path ->
      pure ()
    _ ->
      pure ()

collectDataConstructors :: (Monad m) => ProtoDefinition a Kind () -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) ()
collectDataConstructors =
  \case
    ProtoDType loc _ ProtoTypeDefinition{..} ->
      forM_ protoOtypeDefinitionConstructors $
        \ctor -> do
          entry <- dataConstructorEntry loc ctorSet ctor
          case entry of
            ProtoDataConstructorEntry
              { protoOdataConstructorEntryConstructor = DataConstructor{..}
              } -> do
                insertNameEntry constructorName (ProtoNName constructorName constructorScheme)
                insertExportedName constructorName
                insertDataConstructor constructorName entry
     where
      ctorSet = Set.fromList (for protoOtypeDefinitionConstructors constructorName)
    -- TODO
    ProtoDImport loc path items ->
      pure ()
    -- TODO
    ProtoDQualifiedImport loc path ->
      pure ()
    _ ->
      pure ()

dataConstructorEntry :: (Monad m) => a -> Set Name -> DataConstructor Parameter Kind (Type Parameter Kind) -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) (ProtoDataConstructorEntry a)
dataConstructorEntry loc constructorSet DataConstructor{..} = do
  s <- instantiateScheme constructorScheme
  pure $
    ProtoDataConstructorEntry
      { protoOdataConstructorEntryMetaData = loc
      , protoOdataConstructorEntryName = constructorName
      , protoOdataConstructorEntryConstructor =
          DataConstructor
            { constructorScheme = s
            , ..
            }
      , protoOdataConstructorEntryConstructorSet = constructorSet
      }

collectTraits :: (Monad m) => ProtoDefinition a Kind t -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) ()
collectTraits =
  \case
    ProtoDTrait loc name ProtoTraitDefinition{..} -> do
      insertNameEntry name (ProtoNTrait name)
      insertExportedName name
      insertTrait name entry
     where
      entry =
        ProtoTraitEntry
          { protoOtraitEntryMetadata = loc
          , protoOtraitEntryName = name
          , protoOtraitEntryParameter = protoOtraitDefinitionParameter
          , protoOtraitEntryConstraints = protoOtraitDefinitionConstraints
          , protoOtraitEntryInterface = Environment.fromList protoOtraitDefinitionInterface
          }
    -- TODO
    ProtoDImport loc path items ->
      pure ()
    -- TODO
    ProtoDQualifiedImport loc path ->
      pure ()
    _ ->
      pure ()

collectTraitsInterface :: (Monad m) => ProtoDefinition a Kind t -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) ()
collectTraitsInterface =
  \case
    ProtoDTrait loc name ProtoTraitDefinition{..} -> do
      forM_ protoOtraitDefinitionInterface $
        \(entryName, entryScheme) -> do
          s <- instantiateScheme entryScheme
          insertNameEntry entryName (ProtoNName entryName s)
          exportList <- ask
          let insertName = modify (Build.insertBuildExportedName entryName)
          case exportList of
            ExportAll ->
              insertName
            Exports exports
              | exports `includesName` entryName || exports `includesName` name ->
                  insertName
            _ ->
              pure ()
    -- TODO
    ProtoDImport loc path items ->
      pure ()
    -- TODO
    ProtoDQualifiedImport loc path ->
      pure ()
    _ ->
      pure ()

collectInstances :: (Monad m) => ProtoDefinition a Kind () -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) ()
collectInstances =
  \case
    ProtoDInstance _ ProtoInstanceDefinition{..} ->
      forM_ protoOinstanceDefinitionImplementations $
        \case
          ProtoDLet _ name _ -> do
            insertNameEntry instanceName (PRotoNPlaceholder instanceName)
            insertExportedName instanceName
           where
            instanceName = instanceLabel (instanceDefinitionTrait ProtoInstanceDefinition{..}) name
          ProtoDFunction _ name _ -> do
            insertNameEntry instanceName (PRotoNPlaceholder instanceName)
            insertExportedName instanceName
           where
            instanceName = instanceLabel (instanceDefinitionTrait ProtoInstanceDefinition{..}) name
          _ ->
            pure ()
    -- TODO
    ProtoDImport loc path items ->
      pure ()
    -- TODO
    ProtoDQualifiedImport loc path ->
      pure ()
    _ ->
      pure ()

collectPlaceholders :: (Monad m) => ProtoDefinition a Kind () -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) ()
collectPlaceholders =
  \case
    ProtoDFunction _ name ProtoFunctionDefinition{..} -> do
      insertNameEntry name (PRotoNPlaceholder name)
      insertExportedName name
    ProtoDLet _ name ProtoLetDefinition{..} -> do
      insertNameEntry name (PRotoNPlaceholder name)
      insertExportedName name
    ProtoDFold _ name -> do
      insertNameEntry name (PRotoNPlaceholder name)
      insertExportedName name
    _ ->
      pure ()

instantiateScheme :: (Monad m) => Scheme Parameter Kind (Type Parameter Kind) -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) IndexedScheme
instantiateScheme Forall{..} = do
  lift $ lift $ do
    env <- protoOinstantiateTypeIndexes schemeTypeVariables
    flip runReaderT (Environment.fromList env) $
      Forall
        <$> toIndexed schemeTypeVariables
        <*> toIndexed schemeTraits
        <*> toIndexed schemeTypeBody
