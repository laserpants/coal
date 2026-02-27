{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoCompiler.ProtoBuild.ProtoPrep (
  protoOprepareBuild,
  protoOreplacePlaceholders,
) where

import Control.Monad (when)
import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Language
import Coal.Language.Module.Export (Export (..), includesName)
import Coal.Language.Module.Import (Import (..))
import Coal.Language.Module.Path (Path (..), principalPath)
import Coal.ProtoCompiler.ProtoBuild
import qualified Coal.ProtoCompiler.ProtoBuild as Build
import Coal.ProtoCompiler.ProtoBuild.ProtoNameEntry
import Coal.ProtoCompiler.ProtoStack (ProtoCompilerT (..), insertBuildC, protoOgetCurrentBuildC, protoOinsertNameC)
import Coal.ProtoCompiler.ProtoState
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule (ModuleExportList (..), ProtoModule (..))
import Coal.ProtoTypeSystem.Parameterized (ProtoParameterized (..), ToIndexed (..))
import Coal.TypeSystem.Substitution (Substitutable (apply), mapsTo, normalizeScheme)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.State (StateT, execStateT, get, gets, modify)
import Control.Monad.Trans (lift)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Extras (Name, for, forM, forM_, traverse_)

insertNameEntry :: (Monad m) => ProtoNameEntry -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) m) ()
insertNameEntry entry = modify (Build.insertBuildNameEntry entry)

insertDataConstructor :: (Monad m) => Name -> ProtoDataConstructorEntry a -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) m) ()
insertDataConstructor name entry = modify (Build.insertBuildDataConstructor name entry)

insertTypeConstructor :: (Monad m) => Name -> ProtoTypeConstructorEntry a -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) m) ()
insertTypeConstructor name entry = modify (Build.insertBuildTypeConstructor name entry)

insertTrait :: (Monad m) => Name -> ProtoTraitEntry a -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) m) ()
insertTrait name entry = modify (Build.insertBuildTrait name entry)

insertInstance :: (Monad m) => Name -> IndexedType -> ProtoInstanceEntry a -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) m) ()
insertInstance name t entry = modify (Build.insertBuildInstance name t entry)

insertAlias :: (Monad m) => Name -> ProtoAliasEntry a -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) m) ()
insertAlias name entry = modify (Build.insertBuildAlias name entry)

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

protoOprepareBuild :: (Monad m, Show a) => ProtoModule a Kind () -> ProtoCompilerT m a ()
protoOprepareBuild ProtoModule{..} = do
  build <-
    execStateT
      (runReaderT (protoOprepareDefinitions protoOmoduleDefinitions) protoOmoduleExportList)
      protoOemptyBuild
        { protoObuildPath = protoOmodulePath
        }
  insertBuildC build

protoOprepareDefinitions :: (Monad m, Show a) => [ProtoDefinition a Kind ()] -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) ()
protoOprepareDefinitions defs = do
  -- Collect type constructors
  traverse_ collectTypeConstructors defs
  -- Collect type aliases
  traverse_ collectTypeAliases defs
  -- Collect data constructors
  traverse_ collectDataConstructors defs
  -- expand exports
  exports <- expandExports
  local (const exports) $ do
    -- Collect traits
    traverse_ collectTraits defs
    -- Collect trait interfaces
    traverse_ collectTraitsInterface defs
    -- Collect instances
    traverse_ collectInstances defs
    -- Collect imports
    traverse_ collectImports defs
    -- Collect placeholders
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
      insertNameEntry (ProtoNType name kind)
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

    -- TODO: remove
    ProtoDImport _ (Path ["Builtin$"]) imports -> do
      pure ()
    ProtoDImport _ path imports -> do
      ProtoBuild{..} <- lift $ lift $ importedBuild path
      forM_ imports $
        \case
          TypeImport _ name _
            | name `elem` protoObuildExportedNames ->
                forM_ (Environment.lookupWithDefault [] name protoObuildNames) $
                  \case
                    ProtoNType{} ->
                      case Environment.lookup name protoObuildTypeConstructors of
                        Nothing ->
                          error "TODO"
                        Just entry ->
                          insertTypeConstructor name entry
                    ProtoNTrait{} ->
                      case Environment.lookup name protoObuildTraits of
                        Nothing ->
                          error "TODO"
                        Just entry ->
                          insertTrait name entry
                    ProtoNTypeAlias{} ->
                      case Environment.lookup name protoObuildAliases of
                        Nothing ->
                          error "TODO"
                        Just entry ->
                          insertAlias name entry
                    _ ->
                      pure ()
            | otherwise ->
                error (show name)
          _ ->
            pure ()
    ProtoDQualifiedImport loc path ->
      pure ()
    _ ->
      pure ()

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
          , protoOaliasEntryParams = fmap parameterName protoOaliasDefinitionParameters
          , protoOaliasEntryType = protoOaliasDefinitionType
          }
    ProtoDImport _ path imports ->
      pure ()
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
                insertNameEntry (ProtoNName constructorName constructorScheme)
                insertExportedName constructorName
                insertDataConstructor constructorName entry
                lift $ lift $ protoOinsertNameC constructorName constructorScheme
     where
      ctorSet = Set.fromList (for protoOtypeDefinitionConstructors constructorName)

    -- TODO: remove
    ProtoDImport _ (Path ["Builtin$"]) imports -> do
      pure ()
    ProtoDImport loc path imports -> do
      ProtoBuild{..} <- lift $ lift $ importedBuild path
      forM_ imports $
        \case
          TypeImport _ name ctors
            | name `elem` protoObuildExportedNames ->
                case Environment.lookup name protoObuildTypeConstructors of
                  Nothing ->
                    pure ()
                  -- error (show (path, name))
                  Just ProtoTypeConstructorEntry{..} ->
                    forM_ dataConstructors $
                      \ctor ->
                        case Environment.lookup ctor protoObuildDataConstructors of
                          Nothing ->
                            error "TODO"
                          Just entry@ProtoDataConstructorEntry{protoOdataConstructorEntryConstructor = DataConstructor{..}, ..} -> do
                            insertDataConstructor ctor entry
                            lift $ lift $ protoOinsertNameC constructorName constructorScheme
                   where
                    dataConstructors
                      | ["*"] == ctors = protoOtypeConstructorEntryDataConstructors
                      -- TODO: remove
                      | null ctors = protoOtypeConstructorEntryDataConstructors
                      | otherwise = ctors
            | otherwise ->
                error "TODO"
          _ ->
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

collectTraits :: (Monad m, Show a) => ProtoDefinition a Kind t -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) ()
collectTraits =
  \case
    ProtoDTrait loc name ProtoTraitDefinition{..} -> do
      insertNameEntry (ProtoNTrait name)
      insertExportedName name
      insertTrait name entry
     where
      entry =
        ProtoTraitEntry
          { protoOtraitEntryMetadata = loc
          , protoOtraitEntryName = name
          , protoOtraitEntryParameter = protoOtraitDefinitionParameter
          , protoOtraitEntryConstraints = protoOtraitDefinitionConstraints
          , protoOtraitEntryInterface = Environment.fromList (fmap traitDefinitionInterfaceEntryToPair protoOtraitDefinitionInterface)
          }
    -- TODO
    ProtoDImport _ (Path ["Builtin$"]) imports -> do
      pure ()
    ProtoDImport loc path imports -> do
      ProtoBuild{..} <- lift $ lift $ importedBuild path
      forM_ imports $
        \case
          TypeImport _ name members
            | name `elem` protoObuildExportedNames ->
                case Environment.lookup name protoObuildTraits of
                  Nothing ->
                    pure ()
                    -- error (show (path, name))
                  Just ProtoTraitEntry{..} -> do
                    insertNameEntry (ProtoNTrait name)
                    insertTrait name ProtoTraitEntry{..}
                    forM_ names $
                      \case
                        member | member `elem` protoObuildExportedNames -> do
                          forM_ (Environment.lookupWithDefault [] member protoObuildNames) $
                            \case
                              info@(ProtoNName _ s) -> do
                                modify (insertBuildNameEntry info)
                                lift $ lift $ protoOinsertNameC name s
                              _ -> do
                                pure ()
                        _ ->
                          pure ()
                   where
                     names 
                      | ["*"] == members = Environment.names protoOtraitEntryInterface
                      -- TODO: remove
                      | null members = Environment.names protoOtraitEntryInterface
                      | otherwise = members
            | otherwise ->
                error "TODO"
          _ ->
            pure ()
    -- TODO
    ProtoDQualifiedImport loc path ->
      pure ()
    _ ->
      pure ()

traitDefinitionInterfaceEntryToPair :: ProtoTraitDefinitionInterfaceEntry Kind -> (Name, Scheme Parameter Kind (Type Parameter Kind))
traitDefinitionInterfaceEntryToPair ProtoTraitDefinitionInterfaceEntry{..} = (protoOtraitDefinitionInterfaceEntryName, protoOtraitDefinitionInterfaceEntryScheme)

collectTraitsInterface :: (Monad m) => ProtoDefinition a Kind t -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) ()
collectTraitsInterface =
  \case
    ProtoDTrait _ name ProtoTraitDefinition{..} ->
      forM_ protoOtraitDefinitionInterface $
        \ProtoTraitDefinitionInterfaceEntry{..} -> do
          (s, _) <- instantiateScheme protoOtraitDefinitionInterfaceEntryScheme
          insertNameEntry (ProtoNName protoOtraitDefinitionInterfaceEntryName s)
          lift $ lift $ protoOinsertNameC protoOtraitDefinitionInterfaceEntryName s
          exportList <- ask
          let insertName = modify (Build.insertBuildExportedName protoOtraitDefinitionInterfaceEntryName)
          case exportList of
            ExportAll ->
              insertName
            Exports exports
              | exports `includesName` protoOtraitDefinitionInterfaceEntryName || exports `includesName` name ->
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
            insertNameEntry (ProtoNPlaceholder instanceName)
            insertExportedName instanceName
           where
            instanceName =
              instanceLabel
                (instanceDefinitionTrait ProtoInstanceDefinition{..})
                name
          ProtoDFunction _ name _ -> do
            insertNameEntry (ProtoNPlaceholder instanceName)
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
      insertNameEntry (ProtoNPlaceholder name)
      insertExportedName name
    ProtoDLet _ name ProtoLetDefinition{..} -> do
      insertNameEntry (ProtoNPlaceholder name)
      insertExportedName name
    ProtoDFold _ name ProtoFoldDefinition{..} -> do
      insertNameEntry (ProtoNPlaceholder name)
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
    pure (normalizeScheme s, env)

instantiateType :: (Monad m) => Type Parameter Kind -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) IndexedType
instantiateType t = lift $ lift $ runReaderT (toIndexed t) mempty

collectImports :: (Monad m) => ProtoDefinition a Kind () -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) ()
collectImports =
  \case
    -- TODO: remove
    ProtoDImport _ (Path ["Builtin$"]) imports -> do
      pure ()
    ProtoDImport _ path imports -> do
      ProtoBuild{..} <- lift $ lift $ importedBuild path
      forM_ imports $
        \case
          NameImport _ name
            | name `elem` protoObuildExportedNames -> do
                forM_ (Environment.lookupWithDefault [] name protoObuildNames) $
                  \case
                    info@(ProtoNName _ s) -> do
                      modify (insertBuildNameEntry info)
                      lift $ lift $ protoOinsertNameC name s
                    _ -> do
                      pure ()
            | otherwise ->
                error "TODO"
          TypeImport _ name _
            | name `elem` protoObuildExportedNames ->
                forM_ (Environment.lookupWithDefault [] name protoObuildNames) $
                  \case
                    info@ProtoNType{} ->
                      modify (insertBuildNameEntry info)
                    info@ProtoNTrait{} ->
                      modify (insertBuildNameEntry info)
                    info@ProtoNTypeAlias{} ->
                      modify (insertBuildNameEntry info)
                    _ ->
                      pure ()
            | otherwise ->
                error "TODO"
    ProtoDQualifiedImport _ path ->
      pure ()
    --      error "!"
    _ ->
      pure ()

importedBuild :: (Monad m) => Path -> ProtoCompilerT m a (ProtoBuild a)
importedBuild path = do
  env <- gets protoOcompilerModules
  case Environment.lookup (principalPath path) env of
    Nothing ->
      error (show path) -- "TODO"
    Just build ->
      return build

protoOreplacePlaceholders :: (Monad m) => ProtoCompilerT m a ()
protoOreplacePlaceholders = do
  build <- protoOgetCurrentBuildC
  store <- gets protoOcompilerNameStore
  newBuild <-
    flip execStateT build $
      forM_ (Environment.toList store) $
        \(name, s) ->
          modify (replaceBuildNameEntry (ProtoNName name s))
  insertBuildC newBuild
