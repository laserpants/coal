{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoCompiler.ProtoBuild.ProtoPrep where

import Coal.Language
import Coal.ProtoCompiler.ProtoBuild
import qualified Coal.ProtoCompiler.ProtoBuild as Build
import Coal.ProtoCompiler.ProtoBuild.ProtoNameEntry
import Coal.ProtoCompiler.ProtoStack (ProtoCompilerT (..))
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule (ModuleExportList (..), ProtoModule (..))
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State (StateT, modify)
import Data.Set (Set)
import qualified Data.Set as Set
import Extras (Name, for, forM_, traverse_)

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

protoOprepareBuild :: (Monad m) => ProtoModule a Kind (Type Parameter Kind) -> ProtoCompilerT m (ProtoBuild a)
protoOprepareBuild ProtoModule{..} = do
  undefined

protoOprepareDefinitions :: (Monad m) => [ProtoDefinition a Kind (Type Parameter Kind)] -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m)) ()
protoOprepareDefinitions defs = do
  -- collect type constructors
  traverse_ collectTypeConstructors defs

  -- collect data constructors
  traverse_ collectDataConstructors defs

  -- collect traits
  traverse_ collectTraits defs

  -- collect instances
  traverse_ collectInstances defs

  -- collect placeholders
  traverse_ collectPlaceholders defs

collectTypeConstructors :: (Monad m) => ProtoDefinition a Kind (Type Parameter Kind) -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m)) ()
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
    ProtoDImport loc path items ->
      undefined
    ProtoDQualifiedImport loc path ->
      undefined
    _ ->
      undefined

collectDataConstructors :: (Monad m) => ProtoDefinition a Kind (Type Parameter Kind) -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m)) ()
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
    ProtoDImport loc path items ->
      undefined
    ProtoDQualifiedImport loc path ->
      undefined
    _ ->
      undefined

dataConstructorEntry :: (Monad m) => a -> Set Name -> DataConstructor Parameter Kind (Type Parameter Kind) -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m)) (ProtoDataConstructorEntry a)
dataConstructorEntry loc constructorSet DataConstructor{..} = do
  s <- instantiateScheme loc constructorScheme
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

instantiateScheme :: (Monad m) => a -> Scheme Parameter Kind (Type Parameter Kind) -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m)) IndexedScheme
instantiateScheme = undefined

collectTraits :: (Monad m) => ProtoDefinition a Kind t -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m)) ()
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
          , protoOtraitEntryName = undefined
          , protoOtraitEntryParameter = undefined
          , protoOtraitEntryRequiredInstances = undefined
          , protoOtraitEntryInterface = undefined
          }
    ProtoDImport loc path items ->
      undefined
    ProtoDQualifiedImport loc path ->
      undefined
    _ ->
      undefined

collectInstances :: (Monad m) => ProtoDefinition a Kind (Type Parameter Kind) -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m)) ()
collectInstances =
  \case
    ProtoDInstance loc ProtoInstanceDefinition{..} ->
      undefined
    ProtoDImport loc path items ->
      undefined
    ProtoDQualifiedImport loc path ->
      undefined
    _ ->
      undefined

collectPlaceholders :: (Monad m) => ProtoDefinition a Kind (Type Parameter Kind) -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m)) ()
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
      undefined
