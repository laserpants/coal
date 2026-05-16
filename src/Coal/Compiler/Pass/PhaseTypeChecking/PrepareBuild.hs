{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Coal.Compiler.Pass.PhaseTypeChecking.PrepareBuild
Description: Build environment population before type inference

This module implements a compiler pass that populates the Build environment by
collecting and cataloging all definitions from a Coal module. It serves as the
foundation for type inference and later compilation stages.

The pass operates in several strictly ordered steps:

1. **Type constructor collection**: Gather all type definitions and their kinds
2. **Data constructor collection**: Gather data constructors with their schemes
3. **Export expansion**: Resolve wildcard exports (Type(*) -> Type(A, B, C))
4. **Trait collection**: Register trait definitions
5. **Trait interface collection**: Register trait member signatures
6. **Instance collection**: Register trait implementations
7. **Builtin instance insertion**: Add compiler-provided instances
8. **Import collection**: Process imports from other modules
9. **Placeholder collection**: Register function/let names for type inference
10. **Qualified name resolution**: Build mapping of local to qualified names

The ordering is critical: data constructors depend on type constructors,
instances depend on traits, and imports depend on all prior definitions being
registered in the imported modules.

Monad stack:
  @ReaderT (ExportList a) (StateT (Build a) (CompilerT a m))@

The Reader provides export context (what names should be exported), the State
accumulates the Build structure, and CompilerT provides access to other modules
and error reporting.
-}
module Coal.Compiler.Pass.PhaseTypeChecking.PrepareBuild (
  passPrepareBuild,
  prepareBuild,
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build
import qualified Coal.Compiler.Build as Build
import Coal.Compiler.Build.NameEntry
import Coal.Compiler.Builtin.Instances (builtinInstances)
import Coal.Compiler.Builtin.Names (builtinNames)
import Coal.Compiler.Error
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Compiler.State
import Coal.Language
import Coal.Language.Module.Export (Export (..), includesName)
import Coal.Language.Module.Import (Import (..))
import Coal.Language.Module.Path (Path (..), principalPath)
import Coal.TypeSystem.Kind.Error (KindError (..))
import Coal.TypeSystem.Parameterized (Parameterized (instantiateTypeIndexes), ToIndexed (toIndexed))
import Coal.TypeSystem.Substitution (apply, normalizeScheme)
import qualified Coal.TypeSystem.Substitution as Substitution
import Control.Monad (unless)
import Control.Monad.Except (MonadError (..), MonadIO)
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

passPrepareBuild :: (MonadIO m) => Pass Metadata m (Module Metadata Kind ()) (Module Metadata Kind ())
passPrepareBuild = Pass{runPass = passImpl}

passImpl :: (MonadIO m) => Module Metadata Kind () -> CompilerT Metadata m (Module Metadata Kind ())
passImpl m = do
  prepareBuild m
  return m

insertNameEntry :: (Monad m) => NameEntry -> ReaderT (ExportList a) (StateT (Build a) m) ()
insertNameEntry entry = modify (Build.insertBuildNameEntry entry)

insertDataConstructor :: (Monad m) => Name -> DataConstructorEntry a -> ReaderT (ExportList a) (StateT (Build a) m) ()
insertDataConstructor name entry = modify (Build.insertBuildDataConstructor name entry)

insertTypeConstructor :: (Monad m) => Name -> TypeConstructorEntry a -> ReaderT (ExportList a) (StateT (Build a) m) ()
insertTypeConstructor name entry = modify (Build.insertBuildTypeConstructor name entry)

insertFold :: (Monad m) => Name -> ReaderT (ExportList a) (StateT (Build a) m) ()
insertFold name = modify (Build.insertBuildFold name)

insertTrait :: (Monad m) => Name -> TraitEntry a -> ReaderT (ExportList a) (StateT (Build a) m) ()
insertTrait name entry = modify (Build.insertBuildTrait name entry)

insertInstance :: (Monad m) => Name -> IndexedType -> InstanceEntry a -> ReaderT (ExportList a) (StateT (Build a) m) ()
insertInstance name t entry = modify (Build.insertBuildInstance name t entry)

insertExportedName :: (Monad m) => Name -> ReaderT (ExportList a) (StateT (Build a) m) ()
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

prepareBuild :: (Monad m, Monoid a) => Module a Kind () -> CompilerT a m ()
prepareBuild Module{..} =
  updateCurrentBuildC $
    \build ->
      execStateT
        (runReaderT (prepareDefinitions moduleDefinitions) moduleExportList)
        build
          { buildPath = modulePath
          }

prepareDefinitions :: (Monad m, Monoid a) => [Definition a Kind ()] -> ReaderT (ExportList a) (StateT (Build a) (CompilerT a m)) ()
prepareDefinitions defs = do
  -- Insert built-in type and data constructors that are always available
  -- These are compiler-provided primitives for lists and natural numbers
  insertNameEntry (NType "List" (KArrow KType KType))
  insertTypeConstructor "List" $
    TypeConstructorEntry
      { typeConstructorEntryMetadata = mempty
      , typeConstructorEntryName = "List"
      , typeConstructorEntryKind = KArrow KType KType
      , typeConstructorEntryDataConstructors = ["::", "mempty"]
      }
  insertNameEntry (NName "Zero" (Forall mempty mempty (TIntrinsic INat)))
  insertDataConstructor "Zero" $
    DataConstructorEntry
      { dataConstructorEntryMetaData = mempty
      , dataConstructorEntryName = "Zero"
      , dataConstructorEntryConstructor =
          DataConstructor
            { constructorName = "Zero"
            , constructorArity = 0
            , constructorScheme = Forall mempty mempty (TIntrinsic INat)
            }
      , dataConstructorEntryConstructorSet = Set.fromList ["Zero", "Succ"]
      }
  insertNameEntry (NName "Succ" (Forall mempty mempty (TIntrinsic INat `TArrow` TIntrinsic INat)))
  insertDataConstructor "Succ" $
    DataConstructorEntry
      { dataConstructorEntryMetaData = mempty
      , dataConstructorEntryName = "Succ"
      , dataConstructorEntryConstructor =
          DataConstructor
            { constructorName = "Succ"
            , constructorArity = 1
            , constructorScheme = Forall mempty mempty (TIntrinsic INat `TArrow` TIntrinsic INat)
            }
      , dataConstructorEntryConstructorSet = Set.fromList ["Zero", "Succ"]
      }

  -- Step 1: Collect type constructors
  -- Must happen first because data constructors reference their parent type
  traverse_ collectTypeConstructors defs

  -- Step 2: Collect data constructors
  -- Depends on type constructors being registered (Step 1)
  traverse_ collectDataConstructors defs

  traverse_ collectFolds defs

  -- Step 3: Expand wildcard exports
  -- Converts Type(*) exports to explicit constructor lists (Type(A, B, C))
  -- Must happen after type/data collection to know what constructors exist
  exports <- expandExports
  local (const exports) $ do
    -- Step 4: Collect trait definitions
    -- Must happen before trait interfaces and instances
    traverse_ collectTraits defs

    -- Step 5: Collect trait interface members
    -- Depends on traits being registered (Step 4)
    traverse_ collectTraitsInterface defs

    -- Step 6: Collect trait instances
    -- Depends on traits being defined (Step 4)
    traverse_ collectInstances defs

    -- Step 7: Insert compiler built-in instances
    -- Standard instances provided by the compiler (e.g., Show for primitives)
    forM_ builtinInstances (modify . uncurry3 insertBuildInstance)

    -- Step 8: Collect imports from other modules
    -- Depends on all prior phases completing in the imported modules
    traverse_ collectImports defs

    -- Step 9: Collect function/let placeholders
    -- Creates entries for definitions that will be type-checked later
    traverse_ collectPlaceholders defs

  build <- get
  qualifiedNames <- traverse (qualifiedImports build) defs
  modify (setQualifiedNames (Environment.fromList (concat qualifiedNames)))

{- |
Generate qualified names for trait instance members.

Given a list of trait instances, this function creates qualified name mappings
for all instance member implementations. For example, if a trait \"Show\" has a
member \"show\" and there's an instance for type \"Int\", this generates a mapping
from the instance member name (e.g., \"Show$Int$show\") to its qualified form.

This is used in import resolution to ensure instance members from imported modules
are properly qualified.
-}
generateQualifiedInstanceNames ::
  (Monad m) =>
  Path ->
  Environment [NameEntry] ->
  [(Name, InstanceMap (InstanceEntry a))] ->
  ReaderT (ExportList a) (StateT (Build a) (CompilerT a m)) [(Name, Name)]
generateQualifiedInstanceNames path nameEnv instances =
  concatForM instances $
    \(traitName, instanceMap) ->
      concatForM (Map.toList instanceMap) $
        \(_, InstanceEntry{..}) -> do
          concatForM (Map.keys instanceEntryTypeSchemes) $
            \member -> do
              let instanceName = instanceLabel (Trait traitName instanceEntryType) member
              concatForM (Environment.lookupWithDefault mempty instanceName nameEnv) $
                \case
                  NName n _ -> do
                    pure [(n, principalPath path <.> n)]
                  _ ->
                    pure mempty

{- |
Generate qualified name mappings for imported definitions.

This function processes import statements and creates a mapping from local names
to their fully qualified names (e.g., "map" -> "List.map"). This is crucial for:
- Name resolution during type inference
- Preventing name conflicts between imports
- Supporting qualified access to imported definitions

The function handles three import types:
1. NameImport: Single name import (e.g., @import List.map@)
   - Maps the name to its qualified form
   - Also maps any trait members if the name has trait constraints
   - Also maps trait instance members

2. TypeImport: Type/trait import with optional member list (e.g., @import List.List(::, mempty)@)
   - Maps data constructors or trait members
   - Handles wildcard (*) to import all members
   - Maps instance members for the type/trait

3. NamespaceImport: Import entire module namespace (e.g., @import namespace List@)
   - Maps all exported names with namespace prefix
   - Result: @List.map@, @List.filter@, etc.
-}
qualifiedImports :: (Monad m) => Build a -> Definition a k t -> ReaderT (ExportList a) (StateT (Build a) (CompilerT a m)) [(Name, Name)]
qualifiedImports Build{..} =
  \case
    DImport _ path imports ->
      concatForM imports $
        \case
          NameImport _ name -> do
            nsb <- concatForM (Environment.lookupWithDefault mempty name buildNames) $
              \case
                (NName _ s) -> do
                  concatForM (Set.toList (schemeTraits s)) $
                    \Trait{..} -> do
                      if Path ["Builtin$"] == path
                        then pure []
                        else do
                          Build{buildTraits = importTraits, buildNames = importNames, buildInstances = importInstances} <- lift $ lift $ importedBuild path
                          case Environment.lookup traitName importTraits of
                            Just TraitEntry{..} -> do
                              let entries = Environment.names traitEntryInterface
                                  ns1 = [(n, principalPath path <.> n) | n <- entries]

                              ns2 <- generateQualifiedInstanceNames path importNames (traitInstances traitName importInstances)

                              pure (ns1 <> ns2)
                            _ ->
                              pure mempty
                _ ->
                  pure mempty

            return $
              [(name, principalPath path <.> name)]
                <> nsb
          TypeImport _ name names ->
            case Environment.lookup name buildTypeConstructors of
              Just TypeConstructorEntry{..} -> do
                let dataConstructors =
                      if ["*"] == names
                        then typeConstructorEntryDataConstructors
                        else names `intersect` typeConstructorEntryDataConstructors
                    ns1 = [(n, principalPath path <.> n) | n <- dataConstructors]
                Build{buildInstances = importInstances} <- lift $ lift $ importedBuild path
                ns2 <- generateQualifiedInstanceNames path buildNames (typeInstances name importInstances)
                pure (ns1 <> ns2)
              _ ->
                case Environment.lookup name buildTraits of
                  Just TraitEntry{..} -> do
                    let entries = Environment.names traitEntryInterface
                        ns1 =
                          [ (n, principalPath path <.> n)
                          | n <- if ["*"] == names then entries else names `intersect` entries
                          ]
                    Build{buildInstances = importInstances} <- lift $ lift $ importedBuild path
                    ns2 <- generateQualifiedInstanceNames path buildNames (traitInstances name importInstances)
                    pure (ns1 <> ns2)
                  _ ->
                    pure mempty
    DNamespaceImport _ path -> do
      Build{buildExportedNames = exportedNames} <- lift $ lift $ importedBuild path
      concatForM (Set.toList exportedNames) $
        \name -> pure [(qualified name path, qualified name path)]
    _ ->
      pure mempty

qualified :: Name -> Path -> Name
qualified name path = principalPath path <> "." <> name

expandExports :: (Monad m) => ReaderT (ExportList a) (StateT (Build a) (CompilerT a m)) (ExportList a)
expandExports = do
  exportList <- ask
  Build{buildDataConstructors} <- get
  case exportList of
    ExportAll ->
      return ExportAll
    Exports exports -> do
      newExports <-
        forM exports $
          \case
            TypeExport loc name [] ->
              case Environment.lookup name buildDataConstructors of
                Nothing -> do
                  lift $ lift $ do
                    path <- gets compilerCurrentPath
                    tellErrors [MissingType name (Path []) (ErrorLocation (principalPath path) loc)]
                  return (TypeExport loc name mempty)
                Just DataConstructorEntry{..} ->
                  return (TypeExport loc name (Set.toList dataConstructorEntryConstructorSet))
            e ->
              return e
      return
        (Exports newExports)

collectTypeConstructors :: (Monad m) => Definition a Kind () -> ReaderT (ExportList a) (StateT (Build a) (CompilerT a m)) ()
collectTypeConstructors =
  \case
    DType loc name TypeDefinition{..} -> do
      insertNameEntry (NType name kind)
      insertExportedName name
      insertTypeConstructor name entry
     where
      kind = foldKindOf KType typeDefinitionParameters
      entry =
        TypeConstructorEntry
          { typeConstructorEntryMetadata = loc
          , typeConstructorEntryName = name
          , typeConstructorEntryKind = kind
          , typeConstructorEntryDataConstructors =
              for typeDefinitionConstructors constructorName
          }

    -- Special case: Builtin$ is a compiler-internal module that should not be processed
    -- It's used for bootstrapping and its definitions are handled separately
    DImport _ (Path ["Builtin$"]) _ -> do
      pure ()
    DImport _ path imports -> do
      Build{..} <- lift $ lift $ importedBuild path
      forM_ imports $
        \case
          TypeImport loc name _
            | name `elem` buildExportedNames -> do
                build <- lift $ lift $ importedBuild path
                found <- insertTypeName build loc name
                unless found $ do
                  currentPath <- lift $ lift $ gets compilerCurrentPath
                  lift $ lift $ tellErrors [MissingType name path (ErrorLocation (principalPath currentPath) loc)]
            | otherwise ->
                pure ()
          _ ->
            pure ()
    DNamespaceImport _ _ ->
      pure ()
    _ ->
      pure ()

insertTypeName :: (Monad m) => Build a -> a -> Name -> ReaderT (ExportList a) (StateT (Build a) (CompilerT a m)) Bool
insertTypeName Build{..} loc name =
  or <$> forM (Environment.lookupWithDefault mempty name buildNames) go
 where
  go =
    \case
      NType{} ->
        case Environment.lookup name buildTypeConstructors of
          Nothing -> do
            path <- lift $ lift $ gets compilerCurrentPath
            lift $ lift $ tellErrors [MissingType name (Path []) (ErrorLocation (principalPath path) loc)]
            return False
          Just entry -> do
            insertTypeConstructor name entry
            return True
      NTrait{} ->
        case Environment.lookup name buildTraits of
          Nothing -> do
            path <- lift $ lift $ gets compilerCurrentPath
            lift $ lift $ tellErrors [MissingType name (Path []) (ErrorLocation (principalPath path) loc)]
            return False
          Just entry -> do
            insertTrait name entry
            return True
      NTypeAlias{} ->
        case Environment.lookup name buildAliases of
          Nothing -> do
            path <- lift $ lift $ gets compilerCurrentPath
            lift $ lift $ tellErrors [MissingType name (Path []) (ErrorLocation (principalPath path) loc)]
            return False
          Just AliasEntry{..} -> do
            forM_ (constructors aliasEntryType) (insertTypeName Build{..} loc)
            return True
      _ ->
        return False

collectDataConstructors :: (Monad m) => Definition a Kind () -> ReaderT (ExportList a) (StateT (Build a) (CompilerT a m)) ()
collectDataConstructors =
  \case
    DType loc _ TypeDefinition{..} ->
      forM_ typeDefinitionConstructors $
        \ctor -> do
          entry <- dataConstructorEntry loc ctorSet ctor
          case entry of
            DataConstructorEntry
              { dataConstructorEntryConstructor = DataConstructor{..}
              } -> do
                insertNameEntry (NName constructorName constructorScheme)
                insertExportedName constructorName
                insertDataConstructor constructorName entry
                lift $ lift $ insertNameC constructorName constructorScheme
     where
      ctorSet = Set.fromList (for typeDefinitionConstructors constructorName)

    -- Special case: Builtin$ module handling (see collectTypeConstructors)
    DImport _ (Path ["Builtin$"]) _ -> do
      pure ()
    DImport loc path imports -> do
      Build{..} <- lift $ lift $ importedBuild path
      forM_ imports $
        \case
          TypeImport _ name ctors
            | name `elem` buildExportedNames ->
                case Environment.lookup name buildTypeConstructors of
                  Nothing ->
                    pure ()
                  Just TypeConstructorEntry{..} ->
                    forM_ dataConstructors $
                      \ctor ->
                        case Environment.lookup ctor buildDataConstructors of
                          Nothing -> do
                            currentPath <- lift $ lift $ gets compilerCurrentPath
                            lift $ lift $ tellErrors [NoDataConstructorForType ctor name path (ErrorLocation (principalPath currentPath) loc)]
                          Just entry@DataConstructorEntry{dataConstructorEntryMetaData, dataConstructorEntryConstructor = DataConstructor{..}} -> do
                            insertDataConstructor ctor entry
                            lift $ lift $ insertNewName constructorName dataConstructorEntryMetaData constructorScheme
                   where
                    dataConstructors
                      | ["*"] == ctors = typeConstructorEntryDataConstructors
                      | otherwise = ctors
            | otherwise -> do
                currentPath <- lift $ lift $ gets compilerCurrentPath
                lift $ lift $ tellErrors [ImportNotInModule name path (ErrorLocation (principalPath currentPath) loc)]
          _ ->
            pure ()
    -- Namespace imports are handled in collectImports and qualifiedImports
    DNamespaceImport _ _ ->
      pure ()
    _ ->
      pure ()

dataConstructorEntry :: (Monad m) => a -> Set Name -> DataConstructor Parameter Kind (Type Parameter Kind) -> ReaderT (ExportList a) (StateT (Build a) (CompilerT a m)) (DataConstructorEntry a)
dataConstructorEntry loc constructorSet DataConstructor{..} = do
  (s, _) <- instantiateScheme constructorScheme
  pure $
    DataConstructorEntry
      { dataConstructorEntryMetaData = loc
      , dataConstructorEntryName = constructorName
      , dataConstructorEntryConstructor =
          DataConstructor
            { constructorScheme = normalizeScheme s
            , ..
            }
      , dataConstructorEntryConstructorSet = constructorSet
      }

collectTraits :: (Monad m) => Definition a Kind t -> ReaderT (ExportList a) (StateT (Build a) (CompilerT a m)) ()
collectTraits =
  \case
    DTrait loc name TraitDefinition{..} -> do
      insertNameEntry (NTrait name)
      insertExportedName name
      insertTrait name entry
     where
      entry =
        TraitEntry
          { traitEntryMetadata = loc
          , traitEntryName = name
          , traitEntryParameter = traitDefinitionParameter
          , traitEntryConstraints = traitDefinitionConstraints
          , traitEntryInterface = Environment.fromList (fmap traitDefinitionInterfaceEntryToPair traitDefinitionInterface)
          }
    -- Special case: Builtin$ module handling (see collectTypeConstructors)
    DImport _ (Path ["Builtin$"]) _ -> do
      pure ()
    DImport _ path imports -> do
      Build{..} <- lift $ lift $ importedBuild path
      forM_ imports $
        \case
          TypeImport loc name names
            | name `elem` buildExportedNames ->
                case Environment.lookup name buildTraits of
                  Nothing ->
                    case Environment.lookup name buildTypeConstructors of
                      Just TypeConstructorEntry{} -> do
                        qualifiedInstanceNames (typeInstances name buildInstances)
                      Nothing ->
                        pure ()
                  Just TraitEntry{..} -> do
                    insertNameEntry (NTrait name)
                    insertTrait name TraitEntry{..}
                    qualifiedInstanceNames (traitInstances name buildInstances)
            | otherwise -> do
                currentPath <- lift $ lift $ gets compilerCurrentPath
                lift $ lift $ tellErrors [ImportNotInModule name path (ErrorLocation (principalPath currentPath) loc)]
           where
            qualifiedInstanceNames instances =
              forM_ instances $
                \(traitName, instanceMap) ->
                  forM_ (Map.toList instanceMap) $
                    \(t, InstanceEntry{..}) -> do
                      insertInstance traitName t InstanceEntry{..}
                      let members =
                            if ["*"] == names
                              then Map.keys instanceEntryTypeSchemes
                              else names `intersect` Map.keys instanceEntryTypeSchemes
                      forM_ members $
                        \member -> do
                          let instanceName = instanceLabel (Trait traitName instanceEntryType) member
                          forM_ (Environment.lookupWithDefault mempty instanceName buildNames) $
                            \case
                              info@(NName n s) -> do
                                insertNameEntry info
                                _ <- lift $ lift $ insertNameC n s
                                pure ()
                              _ ->
                                pure ()
          _ ->
            pure ()
    -- Namespace imports are handled in collectImports and qualifiedImports
    DNamespaceImport _ _ ->
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
    Just name == headConstructor instanceEntryType

traitDefinitionInterfaceEntryToPair :: TraitDefinitionInterfaceEntry Kind -> (Name, Scheme Parameter Kind (Type Parameter Kind))
traitDefinitionInterfaceEntryToPair TraitDefinitionInterfaceEntry{..} = (traitDefinitionInterfaceEntryName, traitDefinitionInterfaceEntryScheme)

collectTraitsInterface :: (Monad m) => Definition a Kind t -> ReaderT (ExportList a) (StateT (Build a) (CompilerT a m)) ()
collectTraitsInterface =
  \case
    DTrait _ name TraitDefinition{..} ->
      forM_ traitDefinitionInterface $
        \TraitDefinitionInterfaceEntry{..} -> do
          let Forall{..} = traitDefinitionInterfaceEntryScheme
          (s, _) <-
            instantiateScheme
              ( Forall
                  schemeTypeVariables
                  (Set.fromList [Trait name (TVariable traitDefinitionParameter)])
                  schemeTypeBody
              )
          let normalizedScheme = normalizeScheme s
          insertNameEntry (NName traitDefinitionInterfaceEntryName normalizedScheme)
          _ <- lift $ lift $ insertNameC traitDefinitionInterfaceEntryName normalizedScheme

          exportList <- ask
          let exportName =
                unless (traitDefinitionInterfaceEntryName `elem` builtinNames) $
                  modify (Build.insertBuildExportedName traitDefinitionInterfaceEntryName)
          case exportList of
            ExportAll ->
              exportName
            Exports exports
              | exports `includesName` traitDefinitionInterfaceEntryName || exports `includesName` name ->
                  exportName
            _ ->
              pure ()
    -- Special case: Builtin$ module handling (see collectTypeConstructors)
    DImport _ (Path ["Builtin$"]) _ -> do
      pure ()
    DImport loc path imports -> do
      Build{..} <- lift $ lift $ importedBuild path
      forM_ imports $
        \case
          TypeImport _ name members
            | name `elem` buildExportedNames ->
                case Environment.lookup name buildTraits of
                  Nothing ->
                    pure ()
                  Just TraitEntry{..} -> do
                    forM_ names $
                      \case
                        member | member `elem` buildExportedNames -> do
                          forM_ (Environment.lookupWithDefault mempty member buildNames) $
                            \case
                              info@(NName _ s) -> do
                                modify (insertBuildNameEntry info)
                                _ <- lift $ lift $ insertNameC member s
                                pure ()
                              _ -> do
                                pure ()
                        _ ->
                          pure ()
                   where
                    names
                      | ["*"] == members = Environment.names traitEntryInterface
                      | otherwise = members
            | otherwise -> do
                currentPath <- lift $ lift $ gets compilerCurrentPath
                lift $ lift $ tellErrors [ImportNotInModule name path (ErrorLocation (principalPath currentPath) loc)]
          _ ->
            pure ()
    -- Namespace imports are handled in collectImports and qualifiedImports
    DNamespaceImport _ _ ->
      pure ()
    _ ->
      pure ()

collectInstances :: (Monad m) => Definition a Kind () -> ReaderT (ExportList a) (StateT (Build a) (CompilerT a m)) ()
collectInstances =
  \case
    DInstance _ InstanceDefinition{..} -> do
      t <- instantiateType instanceDefinitionType
      forM_ instanceDefinitionImplementations $
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

      Build{buildTraits, buildTypeConstructors} <- get
      case Environment.lookup instanceDefinitionTraitName buildTraits of
        Just TraitEntry{..} -> do
          Environment env <- flip Environment.mapMEnvironment traitEntryInterface $
            \entry -> do
              (Forall{..}, env) <- instantiateScheme entry
              case lookup (parameterName traitEntryParameter) env of
                Just TypeIndex{..} -> do
                  let sub = Substitution.mapsTo typeIndexId t
                      vs = apply sub schemeTraits
                      t1 = apply sub schemeTypeBody
                  return $ Forall ((typeIndexesIn vs <> typeIndexesIn t1) \\ typeIndexesIn t) vs t1
                Nothing ->
                  error "Implementation error"

          forM_ (constructors t) $
            \constructor ->
              unless (Environment.contains constructor buildTypeConstructors) $
                lift $
                  lift $ do
                    currentPath <- gets compilerCurrentPath
                    tellErrors [KindError (ENoTypeConstructor constructor) (ErrorLocation (principalPath currentPath) instanceDefinitionMetadata)]
                    throwError TypeError

          let entry =
                InstanceEntry
                  { instanceEntryMetadata = instanceDefinitionMetadata
                  , instanceEntryType = instanceDefinitionType
                  , instanceEntryIndexedType = t
                  , instanceEntryTypeSchemes = normalizeScheme <$> env
                  }
          insertInstance instanceDefinitionTraitName t entry
        Nothing -> do
          currentPath <- lift $ lift $ gets compilerCurrentPath
          lift $ lift $ tellErrors [TraitNotInScope instanceDefinitionTraitName (ErrorLocation (principalPath currentPath) instanceDefinitionMetadata)]
    -- Import all instances from imported modules
    DImport _ (Path ["Builtin$"]) _ -> do
      pure ()
    DImport _ path _imports -> do
      Build{..} <- lift $ lift $ importedBuild path
      -- Import all instances from the imported module
      -- Instances are always imported (like in Haskell) regardless of what names are explicitly imported
      forM_ (Environment.toList buildInstances) $
        \(traitName, instanceMap) ->
          forM_ (Map.toList instanceMap) $
            \(t, InstanceEntry{..}) -> do
              insertInstance traitName t InstanceEntry{..}
              -- Import all instance member implementations
              forM_ (Map.keys instanceEntryTypeSchemes) $ \member -> do
                let instanceName = instanceLabel (Trait traitName instanceEntryType) member
                forM_ (Environment.lookupWithDefault mempty instanceName buildNames) $
                  \case
                    info@(NName n s) -> do
                      insertNameEntry info
                      _ <- lift $ lift $ insertNameC n s
                      pure ()
                    _ ->
                      pure ()
    DNamespaceImport _ path -> do
      Build{..} <- lift $ lift $ importedBuild path
      -- Import all instances from the imported module
      forM_ (Environment.toList buildInstances) $
        \(traitName, instanceMap) ->
          forM_ (Map.toList instanceMap) $
            \(t, InstanceEntry{..}) -> do
              insertInstance traitName t InstanceEntry{..}
              -- Import all instance member implementations
              forM_ (Map.keys instanceEntryTypeSchemes) $ \member -> do
                let instanceName = instanceLabel (Trait traitName instanceEntryType) member
                forM_ (Environment.lookupWithDefault mempty instanceName buildNames) $
                  \case
                    info@(NName n s) -> do
                      insertNameEntry info
                      _ <- lift $ lift $ insertNameC n s
                      pure ()
                    _ ->
                      pure ()
    _ ->
      pure ()

collectPlaceholders :: (Monad m) => Definition a Kind () -> ReaderT (ExportList a) (StateT (Build a) (CompilerT a m)) ()
collectPlaceholders =
  \case
    DFunction _ name _ -> do
      insertNameEntry (NPlaceholder name)
      insertExportedName name
    DLet _ name _ -> do
      insertNameEntry (NPlaceholder name)
      insertExportedName name
    DFold _ name _ -> do
      insertNameEntry (NPlaceholder name)
      insertExportedName name
    _ ->
      pure ()

instantiateScheme :: (Monad m) => Scheme Parameter Kind (Type Parameter Kind) -> ReaderT (ExportList a) (StateT (Build a) (CompilerT a m)) (IndexedScheme, [(Name, TypeIndex Kind)])
instantiateScheme Forall{..} =
  lift $ lift $ do
    env <- instantiateTypeIndexes schemeTypeVariables
    s <-
      flip runReaderT (Environment.fromList env) $
        Forall
          <$> toIndexed schemeTypeVariables
          <*> toIndexed schemeTraits
          <*> toIndexed schemeTypeBody
    return (s, env)

instantiateType :: (Monad m) => Type Parameter Kind -> ReaderT (ExportList a) (StateT (Build a) (CompilerT a m)) IndexedType
instantiateType t = lift $ lift $ runReaderT (toIndexed t) mempty

collectFolds :: (Monad m) => Definition a Kind () -> ReaderT (ExportList a) (StateT (Build a) (CompilerT a m)) ()
collectFolds =
  \case
    DFold _ name _ ->
      insertFold name
    _ ->
      pure ()

collectImports :: (Monad m) => Definition a Kind () -> ReaderT (ExportList a) (StateT (Build a) (CompilerT a m)) ()
collectImports =
  \case
    -- Special case: Builtin$ module handling (see collectTypeConstructors)
    DImport _ (Path ["Builtin$"]) _ -> do
      pure ()
    DImport loc path imports -> do
      Build{..} <- lift $ lift $ importedBuild path
      forM_ imports $
        \case
          NameImport iloc name
            | name `elem` buildExportedNames -> do
                forM_ (Environment.lookupWithDefault mempty name buildNames) $
                  \case
                    info@(NName _ s) -> do
                      modify (insertBuildNameEntry info)
                      lift $ lift $ insertNewName name iloc s
                    _ -> do
                      pure ()
            | otherwise -> do
                currentPath <- lift $ lift $ gets compilerCurrentPath
                lift $ lift $ tellErrors [ImportNotInModule name path (ErrorLocation (principalPath currentPath) iloc)]
          TypeImport _ name _
            | name `elem` buildExportedNames ->
                forM_ (Environment.lookupWithDefault mempty name buildNames) $
                  \case
                    info@NType{} ->
                      modify (insertBuildNameEntry info)
                    info@NTrait{} ->
                      modify (insertBuildNameEntry info)
                    info@NTypeAlias{} ->
                      modify (insertBuildNameEntry info)
                    _ ->
                      pure ()
            | otherwise -> do
                currentPath <- lift $ lift $ gets compilerCurrentPath
                lift $ lift $ tellErrors [ImportNotInModule name path (ErrorLocation (principalPath currentPath) loc)]
    DNamespaceImport _ path -> do
      Build{..} <- lift $ lift $ importedBuild path
      let qualifiedName name = principalPath buildPath <.> name
      forM_ buildExportedNames $
        \exportedName ->
          forM_ (Environment.lookupWithDefault mempty exportedName buildNames) $
            \case
              NName name s -> do
                modify (insertBuildNameEntry (NName (qualifiedName name) s))
                _ <- lift $ lift $ insertNameC (qualifiedName name) s
                pure ()
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
  env <- gets compilerModules
  case Environment.lookup (principalPath path) env of
    Nothing ->
      -- Module not found - this should have been caught earlier during dependency resolution
      -- Return empty build to allow compilation to continue and collect all errors
      return emptyBuild
    Just build ->
      return build

insertNewName :: (Monad m) => Name -> a -> IndexedScheme -> CompilerT a m ()
insertNewName name loc s = do
  r <- insertNameC name s
  unless r $ do
    currentPath <- gets compilerCurrentPath
    tellErrors [NameAlreadyDefined name (ErrorLocation (principalPath currentPath) loc)]
    throwError PreflightFailure
