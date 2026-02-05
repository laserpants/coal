{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoCompiler.ProtoBuild.ProtoPrep (protoOprepareBuild) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Language
import Coal.Language.Module.Export (Export (..), includesName)
import Coal.Language.Module.Import (Import (..))
import Coal.Language.Module.Path (Path (..), principalPath)
import Coal.ProtoCompiler.ProtoBuild
import qualified Coal.ProtoCompiler.ProtoBuild as Build
import Coal.ProtoCompiler.ProtoBuild.ProtoNameEntry
import Coal.ProtoCompiler.ProtoStack (ProtoCompilerT (..))
import Coal.ProtoCompiler.ProtoState
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule (ModuleExportList (..), ProtoModule (..))
import Coal.ProtoTypeSystem.Parameterized (ProtoParameterized (..), ToIndexed (..))
import Coal.TypeSystem.Substitution (Substitutable (apply), mapsTo)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.State (StateT, execStateT, get, gets, modify)
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
    -- collect imports
    traverse_ collectImports defs
    -- collect placeholders
    traverse_ collectPlaceholders defs

-- TODO: Set qualified names?

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
  (s, _) <- instantiateScheme constructorScheme
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
    ProtoDTrait _ name ProtoTraitDefinition{..} -> do
      forM_ protoOtraitDefinitionInterface $
        \(entryName, entryScheme) -> do
          (s, _) <- instantiateScheme entryScheme
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
    ProtoDInstance _ ProtoInstanceDefinition{..} -> do
      t <- instantiateType protoOinstanceDefinitionType
      forM_ protoOinstanceDefinitionImplementations $
        \case
          ProtoDLet _ name _ -> do
            insertNameEntry instanceName (PRotoNPlaceholder instanceName)
            insertExportedName instanceName
           where
            instanceName =
              instanceLabel
                (instanceDefinitionTrait ProtoInstanceDefinition{..})
                name
          ProtoDFunction _ name _ -> do
            insertNameEntry instanceName (PRotoNPlaceholder instanceName)
            insertExportedName instanceName
           where
            instanceName =
              instanceLabel
                (instanceDefinitionTrait ProtoInstanceDefinition{..})
                name
          _ ->
            pure ()

      ProtoBuild{protoObuildTraits} <- get
      case Environment.lookup protoOinstanceDefinitionTraitName protoObuildTraits of
        Just ProtoTraitEntry{..} -> do
          Environment env <- flip Environment.mapMEnvironment protoOtraitEntryInterface $
            \entry -> do
              (Forall{..}, env) <- instantiateScheme entry
              case lookup (parameterName protoOtraitEntryParameter) env of
                Nothing ->
                  error "TODO"
                Just (TypeIndex _ index) -> do
                  let sub = index `mapsTo` t
                      newTraits = apply sub schemeTraits
                      newTypeBody = apply sub schemeTypeBody
                      vars = typeIndexesIn newTraits <> typeIndexesIn newTypeBody
                  pure $ Forall vars newTraits newTypeBody
          let entry =
                ProtoInstanceEntry
                  { protoOinstanceEntryMetadata = protoOinstanceDefinitionMetadata
                  , protoOinstanceEntryType = protoOinstanceDefinitionType
                  , protoOinstanceEntryIndexedType = t
                  , protoOinstanceEntryTypeSchemes = env
                  }
          insertInstance protoOinstanceDefinitionTraitName t entry
        Nothing ->
          error "TODO"
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
    ProtoDFold _ name ProtoFoldDefinition{..} -> do
      insertNameEntry name (PRotoNPlaceholder name)
      insertExportedName name
    _ ->
      pure ()

instantiateScheme :: (Monad m) => Scheme Parameter Kind (Type Parameter Kind) -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) (IndexedScheme, [(Name, TypeIndex Kind)])
instantiateScheme Forall{..} =
  lift $ lift $ do
    env <- protoOinstantiateTypeIndexes schemeTypeVariables
    s <-
      flip runReaderT (Environment.fromList env) $
        Forall
          <$> toIndexed schemeTypeVariables
          <*> toIndexed schemeTraits
          <*> toIndexed schemeTypeBody
    pure (s, env)

instantiateType :: (Monad m) => Type Parameter Kind -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) IndexedType
instantiateType t = lift $ lift $ runReaderT (toIndexed t) mempty

collectImports :: (Monad m) => ProtoDefinition a Kind () -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) ()
collectImports =
  \case
    ProtoDImport _ path imports -> do
      ProtoBuild{..} <- lift $ lift $ importedBuild path
      forM_ imports $
        \case
          NameImport _ name
            | name `elem` protoObuildExportedNames ->
                forM_ (Environment.lookupWithDefault [] name protoObuildNames) $
                  \case
                    info@ProtoNName{} ->
                      modify (insertBuildNameEntry name info)
                    _ ->
                      pure ()
            | otherwise ->
                error "TODO"
          TypeImport _ name _
            | name `elem` protoObuildExportedNames ->
                forM_ (Environment.lookupWithDefault [] name protoObuildNames) $
                  \case
                    info@ProtoNType{} ->
                      modify (insertBuildNameEntry name info)
                    info@ProtoNTrait{} ->
                      modify (insertBuildNameEntry name info)
                    info@ProtoNTypeAlias{} ->
                      modify (insertBuildNameEntry name info)
                    _ ->
                      pure ()
            | otherwise ->
                error "TODO"
    ProtoDQualifiedImport _ path ->
      error "!"
    _ ->
      pure ()

importedBuild :: (Monad m) => Path -> ProtoCompilerT m a (ProtoBuild a)
importedBuild path = do
  env <- gets protoOcompilerModules
  case Environment.lookup (principalPath path) env of
    Nothing ->
      undefined
    Just build ->
      return build
