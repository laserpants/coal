{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoCompiler.ProtoBuild.ProtoPrep (
  protoOprepareBuild,
  protoOreplacePlaceholders,
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Builtin.Instances (protoObuiltinInstances)
import Coal.Language
import Coal.Language.Module (qualified)
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
import Coal.ProtoTypeSystem.Parameterized
import Coal.TypeSystem.Substitution (apply, normalizeScheme)
import qualified Coal.TypeSystem.Substitution as Substitution
import Control.Monad (unless, when)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.State (StateT, execStateT, get, gets, modify)
import Control.Monad.Trans (lift)
import Data.List (intersect)
import qualified Data.Map.Strict as Map
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Data.Tuple.Extra (uncurry3)
import Debug.Trace
import Extras (Name, for, forM, forM_, second, traverse_, (<.>))
import Extras.Control.Monad (concatForM)

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

protoOprepareBuild :: (Monad m, Monoid a) => ProtoModule a Kind () -> ProtoCompilerT m a ()
protoOprepareBuild ProtoModule{..} = do
  -- ProtoBuild{
  --  protoObuildAliases = aliases
  --  protoObuildNames = names
  --  protoObuildExportedNames = exportedNames
  -- } <- protoOgetCurrentBuildC
  build <- protoOgetCurrentBuildC
  newBuild <-
    execStateT
      (runReaderT (protoOprepareDefinitions protoOmoduleDefinitions) protoOmoduleExportList)
      build
        { protoObuildPath = protoOmodulePath
        }
  insertBuildC newBuild

protoOprepareDefinitions :: (Monad m, Monoid a) => [ProtoDefinition a Kind ()] -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) ()
protoOprepareDefinitions defs = do
  insertNameEntry (ProtoNType "List" (KArrow KType KType))
  insertTypeConstructor "List" $
    ProtoTypeConstructorEntry
      { protoOtypeConstructorEntryMetadata = mempty
      , protoOtypeConstructorEntryName = "List"
      , protoOtypeConstructorEntryKind = KArrow KType KType
      , protoOtypeConstructorEntryDataConstructors = ["::", "[]"]
      }
  insertNameEntry (ProtoNName "Zero" (Forall mempty [] (TIntrinsic INat)))
  insertDataConstructor "Zero" $
    ProtoDataConstructorEntry
      { protoOdataConstructorEntryMetaData = mempty
      , protoOdataConstructorEntryName = "Zero"
      , protoOdataConstructorEntryConstructor =
          DataConstructor
            { constructorName = "Zero"
            , constructorArity = 0
            , constructorScheme = Forall mempty [] (TIntrinsic INat)
            }
      , protoOdataConstructorEntryConstructorSet = Set.fromList ["Zero", "Succ"]
      }
  insertNameEntry (ProtoNName "Succ" (Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic INat)))
  insertDataConstructor "Succ" $
    ProtoDataConstructorEntry
      { protoOdataConstructorEntryMetaData = mempty
      , protoOdataConstructorEntryName = "Succ"
      , protoOdataConstructorEntryConstructor =
          DataConstructor
            { constructorName = "Succ"
            , constructorArity = 1
            , constructorScheme = Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic INat)
            }
      , protoOdataConstructorEntryConstructorSet = Set.fromList ["Zero", "Succ"]
      }

  -- Collect type constructors
  traverse_ collectTypeConstructors defs

  --  -- Collect type aliases
  --  traverse_ collectTypeAliases defs

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
    -- Insert built-in instances
    forM_ protoObuiltinInstances (modify . uncurry3 insertBuildInstance)
    -- Collect imports
    traverse_ collectImports defs
    -- Collect placeholders
    traverse_ collectPlaceholders defs

  build <- get
  qualifiedNames <- traverse (qualifiedImports build) defs
  modify (setQualifiedNames (Environment.fromList (concat qualifiedNames)))

qualifiedImports :: (Monad m) => ProtoBuild a -> ProtoDefinition a k t -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) [(Name, Name)]
qualifiedImports ProtoBuild{..} =
  \case
    ProtoDImport _ path imports ->
      concatForM imports $
        \case
          NameImport _ name ->
            pure [(name, principalPath path <.> name)]
          TypeImport _ name names ->
            case Environment.lookup name protoObuildTypeConstructors of
              Just ProtoTypeConstructorEntry{..} -> do
                let dataConstructors =
                      if ["*"] == names
                        then protoOtypeConstructorEntryDataConstructors
                        else names `intersect` protoOtypeConstructorEntryDataConstructors
                    ns1 = [(n, principalPath path <.> n) | n <- dataConstructors]
                ProtoBuild{protoObuildInstances = importInstances} <- lift $ lift $ importedBuild path
                ns2 <- qualifiedInstanceNames (typeInstances name importInstances)
                pure (ns1 <> ns2)
              _ ->
                case Environment.lookup name protoObuildTraits of
                  Just ProtoTraitEntry{..} -> do
                    let entries = Environment.names protoOtraitEntryInterface
                        ns1 =
                          [ (n, principalPath path <.> n)
                          | n <- if ["*"] == names then entries else names `intersect` entries
                          ]
                    ProtoBuild{protoObuildInstances = importInstances} <- lift $ lift $ importedBuild path
                    ns2 <- qualifiedInstanceNames (traitInstances name importInstances)
                    pure (ns1 <> ns2)
                  _ ->
                    pure []
     where
      -- qualifiedInstanceNames :: (Monad m) => Path -> Environment [ProtoNameEntry] -> [(Name, InstanceMap (ProtoInstanceEntry a))] -> m [(Name, Name)]
      qualifiedInstanceNames instances =
        concatForM instances $
          \(traitName, instanceMap) ->
            concatForM (Map.toList instanceMap) $
              \(t, ProtoInstanceEntry{..}) -> do
                concatForM (Map.keys protoOinstanceEntryTypeSchemes) $
                  \member -> do
                    let instanceName = instanceLabel (Trait traitName protoOinstanceEntryType) member
                    concatForM (Environment.lookupWithDefault [] instanceName protoObuildNames) $
                      \case
                        ProtoNName n _ -> do
                          pure [(n, principalPath path <.> n)]
                        _ ->
                          pure []
    ProtoDQualifiedImport _ path -> do
      ProtoBuild{protoObuildExportedNames = exportedNames} <- lift $ lift $ importedBuild path
      concatForM (Set.toList exportedNames) $
        \name ->
          --          concatForM (fromMaybe [] $ Environment.lookup name importedNames) $
          --            \case
          --              ProtoNName{} -> do
          --                when (Path ["List"] == path) $
          --                  traceShowM (qualified name path)
          pure [(qualified name path, qualified name path)]
    --              _ ->
    --                pure []
    _ ->
      pure []

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
          TypeImport loc name _
            | name `elem` protoObuildExportedNames -> do
                build <- lift $ lift $ importedBuild path
                found <- insertTypeName build loc name
                unless found $ do
                  error "TODO"

            -- throwError PreflightFailure

            -- found <- insertTypeName path loc name
            -- unless found $ do
            --  tellErrors [MissingType name path (ErrorLocation this loc)]
            --  throwError PreflightFailure

            | otherwise ->
                pure () -- error (show name)
          _ ->
            pure ()
    ProtoDQualifiedImport loc path ->
      pure ()
    _ ->
      pure ()

insertTypeName :: (Monad m) => ProtoBuild a -> a -> Name -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) Bool
insertTypeName ProtoBuild{..} loc name =
  or <$> forM (Environment.lookupWithDefault [] name protoObuildNames) go
 where
  go =
    \case
      ProtoNType{} ->
        case Environment.lookup name protoObuildTypeConstructors of
          Nothing ->
            error "TODO"
          Just entry -> do
            insertTypeConstructor name entry
            return True
      ProtoNTrait{} ->
        case Environment.lookup name protoObuildTraits of
          Nothing ->
            error "TODO"
          Just entry -> do
            insertTrait name entry
            return True
      ProtoNTypeAlias{} ->
        case Environment.lookup name protoObuildAliases of
          Nothing ->
            error "TODO"
          Just ProtoAliasEntry{..} -> do
            forM_ (constructors protoOaliasEntryType) (insertTypeName ProtoBuild{..} loc)
            return True
      --            insertAlias name ProtoAliasEntry{..}
      --            forM_ (constructors protoOaliasEntryType) (insertTypeName ProtoBuild{..} loc)
      --            return True
      _ ->
        return False

-- collectTypeAliases :: (Monad m) => ProtoDefinition a Kind () -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) ()
-- collectTypeAliases =
--  \case
--    ProtoDTypeAlias loc name ProtoAliasDefinition{..} -> do
--      insertNameEntry (ProtoNTypeAlias name)
--      insertExportedName name
--      insertAlias name entry
--     where
--      entry =
--        ProtoAliasEntry
--          { protoOaliasEntryMetadata = loc
--          , protoOaliasEntryName = name
--          , protoOaliasEntryParams = protoOaliasDefinitionParameters
--          , protoOaliasEntryType = protoOaliasDefinitionType
--          }
--    ProtoDImport _ path imports ->
--      pure ()
--    ProtoDQualifiedImport loc path ->
--      pure ()
--    _ ->
--      pure ()

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
                      | otherwise = ctors
            | otherwise ->
                error (show name)
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
            { constructorScheme = normalizeScheme s
            , ..
            }
      , protoOdataConstructorEntryConstructorSet = constructorSet
      }

collectTraits :: (Monad m) => ProtoDefinition a Kind t -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) ()
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
    ProtoDImport _ path imports -> do
      ProtoBuild{..} <- lift $ lift $ importedBuild path
      forM_ imports $
        \case
          TypeImport _ name names
            | name `elem` protoObuildExportedNames ->
                case Environment.lookup name protoObuildTraits of
                  Nothing ->
                    case Environment.lookup name protoObuildTypeConstructors of
                      Just ProtoTypeConstructorEntry{} -> do
                        -- traceShowM name
                        qualifiedInstanceNames (typeInstances name protoObuildInstances)
                      Nothing ->
                        pure ()
                  -- error (show (path, name))
                  Just ProtoTraitEntry{..} -> do
                    insertNameEntry (ProtoNTrait name)
                    insertTrait name ProtoTraitEntry{..}
                    qualifiedInstanceNames (traitInstances name protoObuildInstances)
            | otherwise ->
                error "TODO"
           where
            qualifiedInstanceNames instances =
              forM_ instances $
                \(traitName, instanceMap) ->
                  forM_ (Map.toList instanceMap) $
                    \(t, ProtoInstanceEntry{..}) -> do
                      insertInstance traitName t ProtoInstanceEntry{..}
                      let members =
                            if ["*"] == names
                              then Map.keys protoOinstanceEntryTypeSchemes
                              else names `intersect` Map.keys protoOinstanceEntryTypeSchemes
                      forM_ members $
                        \member -> do
                          -- traceShowM member
                          let instanceName = instanceLabel (Trait traitName protoOinstanceEntryType) member
                          forM_ (Environment.lookupWithDefault [] instanceName protoObuildNames) $
                            \case
                              info@(ProtoNName n s) -> do
                                insertNameEntry info
                                lift $ lift $ protoOinsertNameC n s
                              _ ->
                                pure ()
          _ ->
            pure ()
    -- TODO
    ProtoDQualifiedImport loc path ->
      pure ()
    _ ->
      pure ()

traitInstances :: Name -> Environment (InstanceMap a) -> [(Name, InstanceMap a)]
traitInstances name instances = filter ((==) name . fst) (Environment.toList instances)

typeInstances :: Name -> Environment (InstanceMap (ProtoInstanceEntry a)) -> [(Name, InstanceMap (ProtoInstanceEntry a))]
typeInstances name instances = fmap (second (instancesForType name)) (Environment.toList instances)

instancesForType :: Name -> InstanceMap (ProtoInstanceEntry a) -> InstanceMap (ProtoInstanceEntry a)
instancesForType name = Map.filter isType
 where
  isType ProtoInstanceEntry{..} =
    Just name == headConstructor protoOinstanceEntryType

traitDefinitionInterfaceEntryToPair :: ProtoTraitDefinitionInterfaceEntry Kind -> (Name, Scheme Parameter Kind (Type Parameter Kind))
traitDefinitionInterfaceEntryToPair ProtoTraitDefinitionInterfaceEntry{..} = (protoOtraitDefinitionInterfaceEntryName, protoOtraitDefinitionInterfaceEntryScheme)

collectTraitsInterface :: (Monad m) => ProtoDefinition a Kind t -> ReaderT (ModuleExportList a) (StateT (ProtoBuild a) (ProtoCompilerT m a)) ()
collectTraitsInterface =
  \case
    ProtoDTrait _ name ProtoTraitDefinition{..} ->
      forM_ protoOtraitDefinitionInterface $
        \ProtoTraitDefinitionInterfaceEntry{..} -> do
          let Forall{..} = protoOtraitDefinitionInterfaceEntryScheme
          (s, _) <-
            instantiateScheme
              ( Forall
                  schemeTypeVariables
                  [Trait name (TVariable protoOtraitDefinitionParameter)]
                  schemeTypeBody
              )
          let normalizedScheme = normalizeScheme s
          insertNameEntry (ProtoNName protoOtraitDefinitionInterfaceEntryName normalizedScheme)
          lift $ lift $ protoOinsertNameC protoOtraitDefinitionInterfaceEntryName normalizedScheme

          exportList <- ask
          let exportName =
                unless (protoOtraitDefinitionInterfaceEntryName `elem` builtinNames) $
                  modify (Build.insertBuildExportedName protoOtraitDefinitionInterfaceEntryName)
          case exportList of
            ExportAll ->
              exportName
            Exports exports
              | exports `includesName` protoOtraitDefinitionInterfaceEntryName || exports `includesName` name ->
                  exportName
            _ ->
              pure ()
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
                    forM_ names $
                      \case
                        member | member `elem` protoObuildExportedNames -> do
                          forM_ (Environment.lookupWithDefault [] member protoObuildNames) $
                            \case
                              info@(ProtoNName _ s) -> do
                                modify (insertBuildNameEntry info)
                                lift $ lift $ protoOinsertNameC member s
                              _ -> do
                                pure ()
                        _ ->
                          pure ()
                   where
                    names
                      | ["*"] == members = Environment.names protoOtraitEntryInterface
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
                Just TypeIndex{..} -> do
                  let sub = Substitution.mapsTo typeIndexId t
                      vs = apply sub schemeTraits
                      t1 = apply sub schemeTypeBody
                  return $ Forall ((typeIndexesIn vs <> typeIndexesIn t1) \\ typeIndexesIn t) vs t1
                Nothing ->
                  error "Implementation error"

          -- case lookup (parameterName protoOtraitEntryParameter) env of
          --  Nothing ->
          --    error "TODO"
          --  Just (TypeIndex _ index) -> do
          --    let sub = index `mapsTo` t
          --        newTraits = apply sub schemeTraits
          --        newTypeBody = apply sub schemeTypeBody
          --        vars = typeIndexesIn newTraits <> typeIndexesIn newTypeBody
          --    pure $ Forall vars newTraits newTypeBody
          let entry =
                ProtoInstanceEntry
                  { protoOinstanceEntryMetadata = protoOinstanceDefinitionMetadata
                  , protoOinstanceEntryType = protoOinstanceDefinitionType
                  , protoOinstanceEntryIndexedType = t
                  , protoOinstanceEntryTypeSchemes = normalizeScheme <$> env
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
    return (s, env)

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
                -- error "TODO"
                error (show (name, protoObuildExportedNames))
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
    ProtoDQualifiedImport _ path -> do
      ProtoBuild{..} <- lift $ lift $ importedBuild path
      let qualifiedName name = principalPath protoObuildPath <.> name
      forM_ protoObuildExportedNames $
        \exportedName ->
          forM_ (Environment.lookupWithDefault [] exportedName protoObuildNames) $
            \case
              ProtoNName name s -> do
                modify (insertBuildNameEntry (ProtoNName (qualifiedName name) s))
                lift $ lift $ protoOinsertNameC (qualifiedName name) s
              ProtoNType name k ->
                modify (insertBuildNameEntry (ProtoNType (qualifiedName name) k))
              ProtoNTrait name ->
                modify (insertBuildNameEntry (ProtoNTrait (qualifiedName name)))
              ProtoNTypeAlias name ->
                modify (insertBuildNameEntry (ProtoNTypeAlias (qualifiedName name)))
              ProtoNPlaceholder{} ->
                pure ()
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
