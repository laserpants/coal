{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Build.Prep (
  protoOprepareBuild,
  protoOreplacePlaceholders,
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build
import qualified Coal.Compiler.Build as Build
import Coal.Compiler.Build.NameEntry
import Coal.Compiler.Builtin.Instances (protoObuiltinInstances)
import Coal.Compiler.Stack
import Coal.Compiler.State
import Coal.Language
import Coal.Language.Definition
import Coal.Language.Module (Module (..), ModuleExportList (..))
import Coal.Language.Module.Export (Export (..), includesName)
import Coal.Language.Module.Import (Import (..))
import Coal.Language.Module.Path (Path (..), principalPath)
import Coal.TypeSystem.Parameterized
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
import Extras (Name, for, forM, forM_, second, traverse_, (<.>))
import Extras.Control.Monad (concatForM)

insertNameEntry :: (Monad m) => NameEntry -> ReaderT (ModuleExportList a) (StateT (Build a) m) ()
insertNameEntry entry = modify (Build.insertBuildNameEntry entry)

insertDataConstructor :: (Monad m) => Name -> DataConstructorEntry a -> ReaderT (ModuleExportList a) (StateT (Build a) m) ()
insertDataConstructor name entry = modify (Build.insertBuildDataConstructor name entry)

insertTypeConstructor :: (Monad m) => Name -> TypeConstructorEntry a -> ReaderT (ModuleExportList a) (StateT (Build a) m) ()
insertTypeConstructor name entry = modify (Build.insertBuildTypeConstructor name entry)

insertTrait :: (Monad m) => Name -> TraitEntry a -> ReaderT (ModuleExportList a) (StateT (Build a) m) ()
insertTrait name entry = modify (Build.insertBuildTrait name entry)

insertInstance :: (Monad m) => Name -> IndexedType -> InstanceEntry a -> ReaderT (ModuleExportList a) (StateT (Build a) m) ()
insertInstance name t entry = modify (Build.insertBuildInstance name t entry)

insertAlias :: (Monad m) => Name -> AliasEntry a -> ReaderT (ModuleExportList a) (StateT (Build a) m) ()
insertAlias name entry = modify (Build.insertBuildAlias name entry)

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

protoOprepareBuild :: (Monad m, Monoid a) => Module a Kind () -> CompilerT a m ()
protoOprepareBuild Module{..} = do
  build <- protoOgetCurrentBuildC
  newBuild <-
    execStateT
      (runReaderT (protoOprepareDefinitions protoOmoduleDefinitions) protoOmoduleExportList)
      build
        { protoObuildPath = protoOmodulePath
        }
  insertBuildC newBuild

protoOprepareDefinitions :: (Monad m, Monoid a) => [Definition a Kind ()] -> ReaderT (ModuleExportList a) (StateT (Build a) (CompilerT a m)) ()
protoOprepareDefinitions defs = do
  insertNameEntry (NType "List" (KArrow KType KType))
  insertTypeConstructor "List" $
    TypeConstructorEntry
      { protoOtypeConstructorEntryMetadata = mempty
      , protoOtypeConstructorEntryName = "List"
      , protoOtypeConstructorEntryKind = KArrow KType KType
      , protoOtypeConstructorEntryDataConstructors = ["::", "mempty"]
      }
  insertNameEntry (NName "Zero" (Forall mempty mempty (TIntrinsic INat)))
  insertDataConstructor "Zero" $
    DataConstructorEntry
      { protoOdataConstructorEntryMetaData = mempty
      , protoOdataConstructorEntryName = "Zero"
      , protoOdataConstructorEntryConstructor =
          DataConstructor
            { constructorName = "Zero"
            , constructorArity = 0
            , constructorScheme = Forall mempty mempty (TIntrinsic INat)
            }
      , protoOdataConstructorEntryConstructorSet = Set.fromList ["Zero", "Succ"]
      }
  insertNameEntry (NName "Succ" (Forall mempty mempty (TIntrinsic INat `TArrow` TIntrinsic INat)))
  insertDataConstructor "Succ" $
    DataConstructorEntry
      { protoOdataConstructorEntryMetaData = mempty
      , protoOdataConstructorEntryName = "Succ"
      , protoOdataConstructorEntryConstructor =
          DataConstructor
            { constructorName = "Succ"
            , constructorArity = 1
            , constructorScheme = Forall mempty mempty (TIntrinsic INat `TArrow` TIntrinsic INat)
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

qualifiedImports :: (Monad m) => Build a -> Definition a k t -> ReaderT (ModuleExportList a) (StateT (Build a) (CompilerT a m)) [(Name, Name)]
qualifiedImports Build{..} =
  \case
    DImport _ path imports ->
      concatForM imports $
        \case
          NameImport _ name ->
            pure [(name, principalPath path <.> name)]
          TypeImport _ name names ->
            case Environment.lookup name protoObuildTypeConstructors of
              Just TypeConstructorEntry{..} -> do
                let dataConstructors =
                      if ["*"] == names
                        then protoOtypeConstructorEntryDataConstructors
                        else names `intersect` protoOtypeConstructorEntryDataConstructors
                    ns1 = [(n, principalPath path <.> n) | n <- dataConstructors]
                Build{protoObuildInstances = importInstances} <- lift $ lift $ importedBuild path
                ns2 <- qualifiedInstanceNames (typeInstances name importInstances)
                pure (ns1 <> ns2)
              _ ->
                case Environment.lookup name protoObuildTraits of
                  Just TraitEntry{..} -> do
                    let entries = Environment.names protoOtraitEntryInterface
                        ns1 =
                          [ (n, principalPath path <.> n)
                          | n <- if ["*"] == names then entries else names `intersect` entries
                          ]
                    Build{protoObuildInstances = importInstances} <- lift $ lift $ importedBuild path
                    ns2 <- qualifiedInstanceNames (traitInstances name importInstances)
                    pure (ns1 <> ns2)
                  _ ->
                    pure mempty
     where
      -- qualifiedInstanceNames :: (Monad m) => Path -> Environment [NameEntry] -> [(Name, InstanceMap (InstanceEntry a))] -> m [(Name, Name)]
      qualifiedInstanceNames instances =
        concatForM instances $
          \(traitName, instanceMap) ->
            concatForM (Map.toList instanceMap) $
              \(t, InstanceEntry{..}) -> do
                concatForM (Map.keys protoOinstanceEntryTypeSchemes) $
                  \member -> do
                    let instanceName = instanceLabel (Trait traitName protoOinstanceEntryType) member
                    concatForM (Environment.lookupWithDefault mempty instanceName protoObuildNames) $
                      \case
                        NName n _ -> do
                          pure [(n, principalPath path <.> n)]
                        _ ->
                          pure mempty
    DNamespaceImport _ path -> do
      Build{protoObuildExportedNames = exportedNames} <- lift $ lift $ importedBuild path
      concatForM (Set.toList exportedNames) $
        \name ->
          --          concatForM (fromMaybe mempty $ Environment.lookup name importedNames) $
          --            \case
          --              NName{} -> do
          --                when (Path ["List"] == path) $
          --                  traceShowM (qualified name path)
          pure [(qualified name path, qualified name path)]
    --              _ ->
    --                pure mempty
    _ ->
      pure mempty

qualified :: Name -> Path -> Name
qualified name path = principalPath path <> "." <> name

expandExports :: (Monad m) => ReaderT (ModuleExportList a) (StateT (Build a) (CompilerT a m)) (ModuleExportList a)
expandExports = do
  exportList <- ask
  Build{protoObuildDataConstructors} <- get
  case exportList of
    ExportAll ->
      return ExportAll
    Exports exports -> do
      newExports <-
        forM exports $
          \case
            TypeExport loc name mempty ->
              case Environment.lookup name protoObuildDataConstructors of
                Nothing ->
                  error "TODO"
                Just DataConstructorEntry{..} ->
                  return (TypeExport loc name (Set.toList protoOdataConstructorEntryConstructorSet))
            e ->
              return e
      return
        (Exports newExports)

collectTypeConstructors :: (Monad m) => Definition a Kind () -> ReaderT (ModuleExportList a) (StateT (Build a) (CompilerT a m)) ()
collectTypeConstructors =
  \case
    DType loc name TypeDefinition{..} -> do
      insertNameEntry (NType name kind)
      insertExportedName name
      insertTypeConstructor name entry
     where
      kind = foldKindOf KType protoOtypeDefinitionParameters
      entry =
        TypeConstructorEntry
          { protoOtypeConstructorEntryMetadata = loc
          , protoOtypeConstructorEntryName = name
          , protoOtypeConstructorEntryKind = kind
          , protoOtypeConstructorEntryDataConstructors =
              for protoOtypeDefinitionConstructors constructorName
          }

    -- TODO: remove
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
    DNamespaceImport loc path ->
      pure ()
    _ ->
      pure ()

insertTypeName :: (Monad m) => Build a -> a -> Name -> ReaderT (ModuleExportList a) (StateT (Build a) (CompilerT a m)) Bool
insertTypeName Build{..} loc name =
  or <$> forM (Environment.lookupWithDefault mempty name protoObuildNames) go
 where
  go =
    \case
      NType{} ->
        case Environment.lookup name protoObuildTypeConstructors of
          Nothing ->
            error "TODO"
          Just entry -> do
            insertTypeConstructor name entry
            return True
      NTrait{} ->
        case Environment.lookup name protoObuildTraits of
          Nothing ->
            error "TODO"
          Just entry -> do
            insertTrait name entry
            return True
      NTypeAlias{} ->
        case Environment.lookup name protoObuildAliases of
          Nothing ->
            error "TODO"
          Just AliasEntry{..} -> do
            forM_ (constructors protoOaliasEntryType) (insertTypeName Build{..} loc)
            return True
      --            insertAlias name AliasEntry{..}
      --            forM_ (constructors protoOaliasEntryType) (insertTypeName Build{..} loc)
      --            return True
      _ ->
        return False

-- collectTypeAliases :: (Monad m) => Definition a Kind () -> ReaderT (ModuleExportList a) (StateT (Build a) (CompilerT a m)) ()
-- collectTypeAliases =
--  \case
--    DTypeAlias loc name AliasDefinition{..} -> do
--      insertNameEntry (NTypeAlias name)
--      insertExportedName name
--      insertAlias name entry
--     where
--      entry =
--        AliasEntry
--          { protoOaliasEntryMetadata = loc
--          , protoOaliasEntryName = name
--          , protoOaliasEntryParams = protoOaliasDefinitionParameters
--          , protoOaliasEntryType = protoOaliasDefinitionType
--          }
--    DImport _ path imports ->
--      pure ()
--    DNamespaceImport loc path ->
--      pure ()
--    _ ->
--      pure ()

collectDataConstructors :: (Monad m) => Definition a Kind () -> ReaderT (ModuleExportList a) (StateT (Build a) (CompilerT a m)) ()
collectDataConstructors =
  \case
    DType loc _ TypeDefinition{..} ->
      forM_ protoOtypeDefinitionConstructors $
        \ctor -> do
          entry <- dataConstructorEntry loc ctorSet ctor
          case entry of
            DataConstructorEntry
              { protoOdataConstructorEntryConstructor = DataConstructor{..}
              } -> do
                insertNameEntry (NName constructorName constructorScheme)
                insertExportedName constructorName
                insertDataConstructor constructorName entry
                lift $ lift $ protoOinsertNameC constructorName constructorScheme
     where
      ctorSet = Set.fromList (for protoOtypeDefinitionConstructors constructorName)

    -- TODO: remove
    DImport _ (Path ["Builtin$"]) imports -> do
      pure ()
    DImport loc path imports -> do
      Build{..} <- lift $ lift $ importedBuild path
      forM_ imports $
        \case
          TypeImport _ name ctors
            | name `elem` protoObuildExportedNames ->
                case Environment.lookup name protoObuildTypeConstructors of
                  Nothing ->
                    pure ()
                  -- error (show (path, name))
                  Just TypeConstructorEntry{..} ->
                    forM_ dataConstructors $
                      \ctor ->
                        case Environment.lookup ctor protoObuildDataConstructors of
                          Nothing ->
                            error "TODO"
                          Just entry@DataConstructorEntry{protoOdataConstructorEntryConstructor = DataConstructor{..}, ..} -> do
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
    DNamespaceImport loc path ->
      pure ()
    _ ->
      pure ()

dataConstructorEntry :: (Monad m) => a -> Set Name -> DataConstructor Parameter Kind (Type Parameter Kind) -> ReaderT (ModuleExportList a) (StateT (Build a) (CompilerT a m)) (DataConstructorEntry a)
dataConstructorEntry loc constructorSet DataConstructor{..} = do
  (s, _) <- instantiateScheme constructorScheme
  pure $
    DataConstructorEntry
      { protoOdataConstructorEntryMetaData = loc
      , protoOdataConstructorEntryName = constructorName
      , protoOdataConstructorEntryConstructor =
          DataConstructor
            { constructorScheme = normalizeScheme s
            , ..
            }
      , protoOdataConstructorEntryConstructorSet = constructorSet
      }

collectTraits :: (Monad m) => Definition a Kind t -> ReaderT (ModuleExportList a) (StateT (Build a) (CompilerT a m)) ()
collectTraits =
  \case
    DTrait loc name TraitDefinition{..} -> do
      insertNameEntry (NTrait name)
      insertExportedName name
      insertTrait name entry
     where
      entry =
        TraitEntry
          { protoOtraitEntryMetadata = loc
          , protoOtraitEntryName = name
          , protoOtraitEntryParameter = protoOtraitDefinitionParameter
          , protoOtraitEntryConstraints = protoOtraitDefinitionConstraints
          , protoOtraitEntryInterface = Environment.fromList (fmap traitDefinitionInterfaceEntryToPair protoOtraitDefinitionInterface)
          }
    -- TODO
    DImport _ (Path ["Builtin$"]) imports -> do
      pure ()
    DImport _ path imports -> do
      Build{..} <- lift $ lift $ importedBuild path
      forM_ imports $
        \case
          TypeImport _ name names
            | name `elem` protoObuildExportedNames ->
                case Environment.lookup name protoObuildTraits of
                  Nothing ->
                    case Environment.lookup name protoObuildTypeConstructors of
                      Just TypeConstructorEntry{} -> do
                        -- traceShowM name
                        qualifiedInstanceNames (typeInstances name protoObuildInstances)
                      Nothing ->
                        pure ()
                  -- error (show (path, name))
                  Just TraitEntry{..} -> do
                    insertNameEntry (NTrait name)
                    insertTrait name TraitEntry{..}
                    qualifiedInstanceNames (traitInstances name protoObuildInstances)
            | otherwise ->
                error "TODO"
           where
            qualifiedInstanceNames instances =
              forM_ instances $
                \(traitName, instanceMap) ->
                  forM_ (Map.toList instanceMap) $
                    \(t, InstanceEntry{..}) -> do
                      insertInstance traitName t InstanceEntry{..}
                      let members =
                            if ["*"] == names
                              then Map.keys protoOinstanceEntryTypeSchemes
                              else names `intersect` Map.keys protoOinstanceEntryTypeSchemes
                      forM_ members $
                        \member -> do
                          -- traceShowM member
                          let instanceName = instanceLabel (Trait traitName protoOinstanceEntryType) member
                          forM_ (Environment.lookupWithDefault mempty instanceName protoObuildNames) $
                            \case
                              info@(NName n s) -> do
                                insertNameEntry info
                                lift $ lift $ protoOinsertNameC n s
                              _ ->
                                pure ()
          _ ->
            pure ()
    -- TODO
    DNamespaceImport loc path ->
      pure ()
    _ ->
      pure ()

traitInstances :: Name -> Environment (InstanceMap a) -> [(Name, InstanceMap a)]
traitInstances name instances = filter ((==) name . fst) (Environment.toList instances)

typeInstances :: Name -> Environment (InstanceMap (InstanceEntry a)) -> [(Name, InstanceMap (InstanceEntry a))]
typeInstances name instances = fmap (second (instancesForType name)) (Environment.toList instances)

instancesForType :: Name -> InstanceMap (InstanceEntry a) -> InstanceMap (InstanceEntry a)
instancesForType name = Map.filter isType
 where
  isType InstanceEntry{..} =
    Just name == headConstructor protoOinstanceEntryType

traitDefinitionInterfaceEntryToPair :: TraitDefinitionInterfaceEntry Kind -> (Name, Scheme Parameter Kind (Type Parameter Kind))
traitDefinitionInterfaceEntryToPair TraitDefinitionInterfaceEntry{..} = (protoOtraitDefinitionInterfaceEntryName, protoOtraitDefinitionInterfaceEntryScheme)

collectTraitsInterface :: (Monad m) => Definition a Kind t -> ReaderT (ModuleExportList a) (StateT (Build a) (CompilerT a m)) ()
collectTraitsInterface =
  \case
    DTrait _ name TraitDefinition{..} ->
      forM_ protoOtraitDefinitionInterface $
        \TraitDefinitionInterfaceEntry{..} -> do
          let Forall{..} = protoOtraitDefinitionInterfaceEntryScheme
          (s, _) <-
            instantiateScheme
              ( Forall
                  schemeTypeVariables
                  (Set.fromList [Trait name (TVariable protoOtraitDefinitionParameter)])
                  schemeTypeBody
              )
          let normalizedScheme = normalizeScheme s
          insertNameEntry (NName protoOtraitDefinitionInterfaceEntryName normalizedScheme)
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
    DImport _ (Path ["Builtin$"]) imports -> do
      pure ()
    DImport loc path imports -> do
      Build{..} <- lift $ lift $ importedBuild path
      forM_ imports $
        \case
          TypeImport _ name members
            | name `elem` protoObuildExportedNames ->
                case Environment.lookup name protoObuildTraits of
                  Nothing ->
                    pure ()
                  -- error (show (path, name))
                  Just TraitEntry{..} -> do
                    forM_ names $
                      \case
                        member | member `elem` protoObuildExportedNames -> do
                          forM_ (Environment.lookupWithDefault mempty member protoObuildNames) $
                            \case
                              info@(NName _ s) -> do
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
    DNamespaceImport loc path ->
      pure ()
    _ ->
      pure ()

collectInstances :: (Monad m) => Definition a Kind () -> ReaderT (ModuleExportList a) (StateT (Build a) (CompilerT a m)) ()
collectInstances =
  \case
    DInstance _ InstanceDefinition{..} -> do
      t <- instantiateType protoOinstanceDefinitionType
      forM_ protoOinstanceDefinitionImplementations $
        \case
          DLet _ name _ -> do
            insertNameEntry (NPlaceholder instanceName)
            insertExportedName instanceName
           where
            instanceName =
              instanceLabel
                (instanceDefinitionTrait InstanceDefinition{..})
                name
          DFunction _ name _ -> do
            insertNameEntry (NPlaceholder instanceName)
            insertExportedName instanceName
           where
            instanceName =
              instanceLabel
                (instanceDefinitionTrait InstanceDefinition{..})
                name
          _ ->
            pure ()

      Build{protoObuildTraits} <- get
      case Environment.lookup protoOinstanceDefinitionTraitName protoObuildTraits of
        Just TraitEntry{..} -> do
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
                InstanceEntry
                  { protoOinstanceEntryMetadata = protoOinstanceDefinitionMetadata
                  , protoOinstanceEntryType = protoOinstanceDefinitionType
                  , protoOinstanceEntryIndexedType = t
                  , protoOinstanceEntryTypeSchemes = normalizeScheme <$> env
                  }
          insertInstance protoOinstanceDefinitionTraitName t entry
        Nothing ->
          error "TODO"
    -- TODO
    DImport loc path items ->
      pure ()
    -- TODO
    DNamespaceImport loc path ->
      pure ()
    _ ->
      pure ()

collectPlaceholders :: (Monad m) => Definition a Kind () -> ReaderT (ModuleExportList a) (StateT (Build a) (CompilerT a m)) ()
collectPlaceholders =
  \case
    DFunction _ name FunctionDefinition{..} -> do
      insertNameEntry (NPlaceholder name)
      insertExportedName name
    DLet _ name LetDefinition{..} -> do
      insertNameEntry (NPlaceholder name)
      insertExportedName name
    DFold _ name FoldDefinition{..} -> do
      insertNameEntry (NPlaceholder name)
      insertExportedName name
    _ ->
      pure ()

instantiateScheme :: (Monad m) => Scheme Parameter Kind (Type Parameter Kind) -> ReaderT (ModuleExportList a) (StateT (Build a) (CompilerT a m)) (IndexedScheme, [(Name, TypeIndex Kind)])
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

instantiateType :: (Monad m) => Type Parameter Kind -> ReaderT (ModuleExportList a) (StateT (Build a) (CompilerT a m)) IndexedType
instantiateType t = lift $ lift $ runReaderT (toIndexed t) mempty

collectImports :: (Monad m) => Definition a Kind () -> ReaderT (ModuleExportList a) (StateT (Build a) (CompilerT a m)) ()
collectImports =
  \case
    -- TODO: remove
    DImport _ (Path ["Builtin$"]) imports -> do
      pure ()
    DImport _ path imports -> do
      Build{..} <- lift $ lift $ importedBuild path
      forM_ imports $
        \case
          NameImport _ name
            | name `elem` protoObuildExportedNames -> do
                forM_ (Environment.lookupWithDefault mempty name protoObuildNames) $
                  \case
                    info@(NName _ s) -> do
                      modify (insertBuildNameEntry info)
                      lift $ lift $ protoOinsertNameC name s
                    _ -> do
                      pure ()
            | otherwise ->
                -- error "TODO"
                error (show (name, protoObuildExportedNames))
          TypeImport _ name _
            | name `elem` protoObuildExportedNames ->
                forM_ (Environment.lookupWithDefault mempty name protoObuildNames) $
                  \case
                    info@NType{} ->
                      modify (insertBuildNameEntry info)
                    info@NTrait{} ->
                      modify (insertBuildNameEntry info)
                    info@NTypeAlias{} ->
                      modify (insertBuildNameEntry info)
                    _ ->
                      pure ()
            | otherwise ->
                error "TODO"
    DNamespaceImport _ path -> do
      Build{..} <- lift $ lift $ importedBuild path
      let qualifiedName name = principalPath protoObuildPath <.> name
      forM_ protoObuildExportedNames $
        \exportedName ->
          forM_ (Environment.lookupWithDefault mempty exportedName protoObuildNames) $
            \case
              NName name s -> do
                modify (insertBuildNameEntry (NName (qualifiedName name) s))
                lift $ lift $ protoOinsertNameC (qualifiedName name) s
              NType name k ->
                modify (insertBuildNameEntry (NType (qualifiedName name) k))
              NTrait name ->
                modify (insertBuildNameEntry (NTrait (qualifiedName name)))
              NTypeAlias name ->
                modify (insertBuildNameEntry (NTypeAlias (qualifiedName name)))
              NPlaceholder{} ->
                pure ()
    _ ->
      pure ()

importedBuild :: (Monad m) => Path -> CompilerT a m (Build a)
importedBuild path = do
  env <- gets protoOcompilerModules
  case Environment.lookup (principalPath path) env of
    Nothing ->
      error (show path) -- "TODO"
    Just build ->
      return build

protoOreplacePlaceholders :: (Monad m) => CompilerT a m ()
protoOreplacePlaceholders = do
  build <- protoOgetCurrentBuildC
  store <- gets protoOcompilerNameStore
  newBuild <-
    flip execStateT build $
      forM_ (Environment.toList store) $
        \(name, s) ->
          modify (replaceBuildNameEntry (NName name s))
  insertBuildC newBuild
