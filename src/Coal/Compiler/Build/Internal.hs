{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Build.Internal (
  buildEnv,
  replacePlaceholders,
  prepareBuild,
  typeConstructorEnv,
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build
import Coal.Compiler.Build.NameEntry
import Coal.Compiler.Builtin.DataConstructors (builtinDataConstructors)
import Coal.Compiler.Builtin.Instances (builtinInstances)
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Coal.TypeSystem.Kind.Inference
import Coal.TypeSystem.Substitution (Substitutable (apply), Substitution, mapsTo)
import Control.Monad.Except (MonadError (throwError), MonadTrans (lift), forM, forM_, unless, when)
import Control.Monad.State (StateT, execStateT, gets, modify, runStateT)
import Data.List (nub, union, (\\))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Extras (Name, groupByKey, (<$$>))
import Extras.Control.Monad (concatForM)

buildEnv :: (Monad m) => CompilerT a m (Environment IndexedScheme)
buildEnv = do
  ModuleBuild{..} <- getCurrentBuildC
  flip execStateT mempty $ do
    forM_ moduleNames $
      \case
        NFunction name s ->
          modify (Environment.insert name s)
        NConstant name s ->
          modify (Environment.insert name s)
        NFold name s ->
          modify (Environment.insert name s)
        NUnfold name s ->
          modify (Environment.insert name s)
        _ ->
          pure ()

replacePlaceholders :: (Monad m) => Environment IndexedScheme -> CompilerT a m ()
replacePlaceholders store =
  updateBuildC $
    \build@ModuleBuild{..} ->
      flip execStateT build $
        forM_ moduleNames $
          \case
            NFunctionPlaceholder name ->
              go name NFunction
            NConstantPlaceholder name ->
              go name NConstant
            NFoldPlaceholder name ->
              go name NFold
            NUnfoldPlaceholder name ->
              go name NUnfold
            _ ->
              pure ()
 where
  go :: (Monad m) => Name -> (Name -> IndexedScheme -> NameEntry) -> StateT (ModuleBuild a) (CompilerT a m) ()
  go name info =
    case Environment.lookup name store of
      Nothing ->
        error "Implementation error"
      Just s ->
        modify $ addName (info name s)

prepareBuild :: (Monad m, Monoid a, Eq a) => Module a Kind () -> CompilerT a m (Module a Kind (), ModuleBuild a)
prepareBuild (Module path exports defs) = do
  flip runStateT emptyModuleBuild $ do
    modify (setPath path)

    inEachDef collectTypeConstructors

    -- Built-in type constructors
    modify $
      insertTypeConstructor "List" (TypeConstructorEntry mempty "List" (KArrow KType KType) [])
        . addName (NType "List" (KArrow KType KType))

    kinds <- typeConstructorEnv
    inEachDef (collectDataConstructors kinds)

    -- Built-in data constructors
    forM_ builtinDataConstructors $
      \(name, info@DataConstructorEntry{dataConstructorEntryConstructor = DataConstructor{..}}) ->
        modify $
          insertDataConstructor name info
            . addName (NDataConstructor name constructorScheme)

    inEachDef (collectTraits kinds)
    traits <- traitEnv

    inEachDef (collectInstances kinds traits)

    -- Built-in instances
    forM_ builtinInstances $
      \(name, t, info) ->
        modify $ insertInstance name t info

    inEachDef collectImportedNames
    inEachDef collectPlaceholders

    exps <- gets (Set.filter (`notElem` builtin) . moduleExports)
    typeExps <- gets (Set.filter (`notElem` builtin) . moduleTypeExports)

    extra <- nub . concat <$> forM defs collectImportedInstances

    let defs1 = [DImport mempty p (ImportName mempty <$> names) | (p, names) <- groupByKey extra]

    if [ExportAll] == exports
      then modify $ setExports (Set.toList exps) . setTypeExports (Set.toList typeExps)
      else
        modify $
          setExports (nameExports exports `union` Set.toList exps)
            . setTypeExports (typeExports exports `union` Set.toList typeExps)

    return (Module path exports (defs1 <> defs))
 where
  builtin =
    Set.fromList
      [ "(%)"
      , "(*)"
      , "(+)"
      , "(-)"
      , "(/)"
      , "(<>)"
      , "(==)"
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
      , "Ordered"
      , "Ordering"
      , "Semigroup"
      , "Some"
      , "compare"
      , "from_int32"
      , "negate"
      ]
  inEachDef = forM_ defs

nameExports :: [Export a] -> [Name]
nameExports exports =
  flip concatMap exports $
    \case
      -- TODO: Rename to ExprExport/ExprImport?
      ExportName _ name ->
        [name]
      ExportType _ _ names ->
        names
      _ ->
        []

typeExports :: [Export a] -> [Name]
typeExports exports =
  flip concatMap exports $
    \case
      ExportType _ name _ ->
        [name]
      _ ->
        []

{-# INLINE pick #-}
pick :: (HasName e) => [Name] -> [e] -> [e]
pick names = filter (\e -> nameOf e `Set.member` ns)
 where
  ns = Set.fromList names

collectNameImports :: (Monad m, HasName e) => Definition a Kind () -> (ModuleBuild a -> [e]) -> StateT (ModuleBuild a) (CompilerT a m) [e]
collectNameImports (DImport _ (Path ["Builtin$"]) _) _ = pure []
collectNameImports (DImport loc path imports) getter = do
  build <- importedModule loc path
  let env = getter build
  pure (pick (nameImports build imports) env)
collectNameImports _ _ = error "Implementation error"

collectTypeImports :: (Monad m, HasName e) => Definition a Kind () -> (ModuleBuild a -> [e]) -> StateT (ModuleBuild a) (CompilerT a m) [e]
collectTypeImports (DImport _ (Path ["Builtin$"]) _) _ = pure []
collectTypeImports (DImport loc path imports) getter = do
  build <- importedModule loc path
  let env = getter build
  pure (pick (typeImports imports) env)
collectTypeImports _ _ = error "Implementation error"

nameImports :: ModuleBuild a -> [Import a] -> [Name]
nameImports ModuleBuild{..} imports =
  flip concatMap imports $
    \case
      ImportName _ name ->
        [name]
      ImportType _ name ["*"] ->
        case Environment.lookup name moduleTypeConstructors of
          Nothing ->
            error "Implementation error"
          Just TypeConstructorEntry{..} ->
            typeConstructorEntryDataConstructors
      ImportType _ _ names ->
        names
      ImportCotype _ name ["*"] ->
        case Environment.lookup name moduleCotypeConstructors of
          Nothing ->
            error "Implementation error"
          Just CotypeConstructorEntry{..} ->
            cotypeConstructorEntryDataAccessors
      ImportCotype _ _ names ->
        names
      ImportTrait _ name ["*"] ->
        case Environment.lookup name moduleTraits of
          Nothing ->
            error "Implementation error"
          Just TraitEntry{..} ->
            Environment.names traitEntryEntries
      ImportTrait _ _ names ->
        names

typeImports :: [Import a] -> [Name]
typeImports imports =
  flip concatMap imports $
    \case
      ImportType _ name _ ->
        [name]
      ImportCotype _ name _ ->
        [name]
      ImportTrait _ name _ ->
        [name]
      _ ->
        []

importedModule :: (Monad m) => a -> Path -> StateT (ModuleBuild a) (CompilerT a m) (ModuleBuild a)
importedModule loc path = do
  env <- lift (gets compilerModules)
  case Environment.lookup (principalPath path) env of
    Nothing -> do
      tellErrors [ModuleNotFound (principalPath path) (ErrorLocation (principalPath path) loc)]
      throwError PreflightFailure
    Just build -> do
      return build

collectTypeConstructors :: (Monad m) => Definition a Kind () -> StateT (ModuleBuild a) (CompilerT a m) ()
collectTypeConstructors =
  \case
    DType loc name def -> do
      modify $
        insertTypeConstructor name info
          . addName (NType name kind_)
          . addTypeExport name
     where
      info@(TypeConstructorEntry _ _ kind_ _) = typeConstructorEntry loc name def
    DCotype loc name def -> do
      modify $
        insertCotypeConstructor name info
          . addName (NCotype name kind_)
          . addTypeExport name
     where
      info@(CotypeConstructorEntry _ _ kind_ _) = cotypeConstructorEntry loc name def
    DTypeAlias loc name alias -> do
      modify $
        insertAlias name (aliasEntry loc name alias)
          . addName (NAlias name)
          . addTypeExport name
    DImport _ (Path ["Builtin$"]) _ ->
      pure ()
    DImport a path imports -> do
      build <- importedModule a path
      this <- lift $ gets (principalPath . compilerCurrentModule)
      forM_ imports $
        \case
          ImportType loc name _ ->
            case Environment.lookup name (exportedTypeConstructors build) of
              Nothing -> do
                tellErrors [MissingType name path (ErrorLocation this loc)]
                throwError PreflightFailure
              Just entry ->
                modify $ insertTypeConstructor name entry
          ImportCotype loc name _ ->
            case Environment.lookup name (exportedCotypeConstructors build) of
              Nothing -> do
                tellErrors [MissingCotype name path (ErrorLocation this loc)]
                throwError PreflightFailure
              Just entry ->
                modify $ insertCotypeConstructor name entry
          _ ->
            pure ()
    DQualifiedImport a path -> do
      build <- importedModule a path
      forM_ (Environment.toList (exportedTypeConstructors build)) $
        \(n, entry) ->
          modify $ insertTypeConstructor (principalPath path <> "." <> n) entry
      forM_ (Environment.toList (exportedCotypeConstructors build)) $
        \(n, entry) ->
          modify $ insertCotypeConstructor (principalPath path <> "." <> n) entry
    _ ->
      pure ()

{-# INLINE foldElems #-}
foldElems :: (Monoid m) => (a -> m -> m) -> Environment a -> m
foldElems f = foldr f mempty . Environment.elems

traitEnv :: (Monad m) => StateT (ModuleBuild a) (CompilerT a m) (Environment (TraitEntry a))
traitEnv = do
  gets (foldElems insertTraitEntry . moduleTraits)
 where
  insertTraitEntry :: TraitEntry a -> Environment (TraitEntry a) -> Environment (TraitEntry a)
  insertTraitEntry info@(TraitEntry _ name _ _ _) = Environment.insert name info

typeConstructorEnv :: (Monad m) => StateT (ModuleBuild a) (CompilerT a m) (Environment Kind)
typeConstructorEnv = do
  env1 <- gets (foldElems insertTypeInfo . moduleTypeConstructors)
  env2 <- gets (foldElems insertCotypeInfo . moduleCotypeConstructors)
  pure (env1 <> env2)
 where
  insertTypeInfo :: TypeConstructorEntry a -> Environment Kind -> Environment Kind
  insertTypeInfo (TypeConstructorEntry _ name kind_ _) = Environment.insert name kind_

  insertCotypeInfo :: CotypeConstructorEntry a -> Environment Kind -> Environment Kind
  insertCotypeInfo (CotypeConstructorEntry _ name kind_ _) = Environment.insert name kind_

collectDataConstructors :: (Monad m) => Environment Kind -> Definition a Kind () -> StateT (ModuleBuild a) (CompilerT a m) ()
collectDataConstructors env =
  \case
    DType loc _ def ->
      forM_ (dataConstructorEntries env loc def) $
        \info@(DataConstructorEntry _ _ DataConstructor{..} _) -> do
          modify $
            addName (NDataConstructor constructorName constructorScheme)
              . insertDataConstructor constructorName info
              . addExport constructorName
    DCotype loc _ def ->
      forM_ (codataAccessorEntries env loc def) $
        \info@(CodataAccessorEntry _ _ CodataAccessor{..}) -> do
          modify $
            addName (NCodataAccessor codataAccessorName codataAccessorScheme)
              . insertCodataAccessor codataAccessorName info
              . addExport codataAccessorName
    DImport _ (Path ["Builtin$"]) _ ->
      pure ()
    DImport a path imports -> do
      build <- importedModule a path
      this <- lift $ gets (principalPath . compilerCurrentModule)
      forM_ imports $
        \case
          ImportType loc name ctors ->
            case Environment.lookup name (exportedTypeConstructors build) of
              Nothing -> do
                tellErrors [MissingType name path (ErrorLocation this loc)]
                throwError PreflightFailure
              Just TypeConstructorEntry{..} -> do
                let missing = ctors \\ typeConstructorEntryDataConstructors
                    importAll = ["*"] == ctors
                unless (importAll || null missing) $
                  forM_ missing $
                    \ctor -> do
                      tellErrors [NoDataConstructorForType ctor name path (ErrorLocation this loc)]
                      throwError PreflightFailure
                forM_ (if importAll then typeConstructorEntryDataConstructors else ctors) $
                  \ctor ->
                    case Environment.lookup ctor (exportedDataConstructors build) of
                      Nothing ->
                        error "Implementation error"
                      Just entry ->
                        modify $ insertDataConstructor ctor entry
          ImportCotype loc name xsors ->
            case Environment.lookup name (exportedCotypeConstructors build) of
              Nothing -> do
                tellErrors [MissingCotype name path (ErrorLocation this loc)]
                throwError PreflightFailure
              Just CotypeConstructorEntry{..} -> do
                let missing = xsors \\ cotypeConstructorEntryDataAccessors
                    importAll = ["*"] == xsors
                unless (importAll || null missing) $
                  forM_ missing $
                    \xsor -> do
                      tellErrors [NoCodataAccessorForCotype xsor name path (ErrorLocation this loc)]
                      throwError PreflightFailure
                forM_ (if importAll then cotypeConstructorEntryDataAccessors else xsors) $
                  \xsor ->
                    case Environment.lookup xsor (exportedCodataAccessors build) of
                      Nothing ->
                        error "Implementation error"
                      Just entry ->
                        modify $ insertCodataAccessor xsor entry
          _ ->
            pure ()
    DQualifiedImport a path -> do
      build <- importedModule a path
      forM_ (Environment.toList (exportedDataConstructors build)) $
        \(n, entry) ->
          modify $ insertDataConstructor (principalPath path <> "." <> n) entry
      forM_ (Environment.toList (exportedCodataAccessors build)) $
        \(n, entry) ->
          modify $ insertCodataAccessor (principalPath path <> "." <> n) entry
    _ ->
      pure ()

collectTraits :: (Monad m) => Environment Kind -> Definition a Kind () -> StateT (ModuleBuild a) (CompilerT a m) ()
collectTraits env =
  \case
    DTrait loc name def -> do
      addTraitEntries env name def'
      modify $
        addName (NTrait name)
          . insertTrait name entry
          . addTypeExport name
     where
      def'@(TraitDef ts p ds) = inferTraitKinds env def
      entry =
        TraitEntry loc name p ts (Environment.fromList ds)
    DImport _ (Path ["Builtin$"]) _ ->
      pure ()
    def@(DImport loc path _) -> do
      ModuleBuild{..} <- importedModule loc path
      traits <- collectTypeImports def exportedTraits
      forM_ traits $
        \info@(TraitEntry _ name _ _ _) -> do
          modify $ insertTrait name info
          forM_ (Environment.toList moduleInstances) $
            \(trait, is) -> do
              when (trait == name) $
                forM_ (Map.toList is) $
                  \(t, InstanceEntry{..}) -> do
                    modify $ insertInstance trait t InstanceEntry{..}
    DQualifiedImport a path -> do
      build <- importedModule a path
      let entries = exportedTraits build
      forM_ entries $
        \entry ->
          modify $ insertTrait (principalPath path <> "." <> traitEntryName entry) entry
    _ ->
      pure ()

addTraitEntries :: (Monad m) => Environment Kind -> Name -> TraitDef Kind -> StateT (ModuleBuild a) (CompilerT a m) ()
addTraitEntries env trait (TraitDef _ p entries) =
  forM_ entries $
    -- TODO
    \(name, Forall _ _ t) ->
      modify $
        addName (NFunction name $ scheme [Trait trait tvar] (toIndexedType env p t))
          . addExport name
 where
  tvar = TVariable (TypeIndex (parameterKind p) 0)

collectInstances :: (Monad m) => Environment Kind -> Environment (TraitEntry a) -> Definition a Kind () -> StateT (ModuleBuild a) (CompilerT a m) ()
collectInstances kinds traits =
  \case
    DInstance loc trait (InstanceDef _ q entries) -> do
      this <- lift $ gets (principalPath . compilerCurrentModule)
      case Environment.lookup trait traits of
        Nothing -> do
          tellErrors [TraitNotInScope trait (ErrorLocation this loc)]
          throwError PreflightFailure
        Just (TraitEntry _ _ p deps dict) -> do
          let inames = definitionName <$> entries
              tnames = Environment.names dict
              extra = inames \\ tnames
              missing = tnames \\ inames
          unless (null extra && null missing) $ do
            forM_ missing $
              \name -> do
                tellErrors [MissingTraitDefinition name trait (ErrorLocation this loc)]
                throwError PreflightFailure
            forM_ extra $
              \name -> do
                tellErrors [UnexpectedTraitDefinition name trait (ErrorLocation this loc)]
                throwError PreflightFailure
          modify $ insertInstance trait t1 (InstanceEntry loc q (toIndexedType kinds p q) env)

          instances <- gets moduleInstances
          forM_ deps $
            \(Trait n t) -> do
              let t2 = toIndexedType kinds t q
              case Environment.lookup n instances of
                Nothing -> do
                  tellErrors [MissingRequiredInstance n t2 (ErrorLocation this loc)]
                  throwError PreflightFailure
                Just res ->
                  case Map.lookup t2 res of
                    Nothing -> do
                      tellErrors [MissingRequiredInstance n t2 (ErrorLocation this loc)]
                      throwError PreflightFailure
                    Just{} ->
                      pure ()
         where
          t1 = toIndexedType kinds p q
          Environment env = Environment.mapEnvironment (substituteInScheme (0 `mapsTo` t1) . toIndexedScheme kinds p) dict
    DImport _ (Path ["Builtin$"]) _ ->
      pure ()
    DImport loc path imports -> do
      ModuleBuild{..} <- importedModule loc path
      forM_ imports $
        \case
          -- TODO: cleanup/DRY
          ImportType _ name _ ->
            forM_ (Environment.toList moduleInstances) $
              \(trait, is) -> do
                forM_ (Map.toList is) $
                  \(t, InstanceEntry{..}) -> do
                    when (headConstructor instanceEntryIndexedType == Just name) $
                      modify $
                        insertInstance trait t InstanceEntry{..}
          _ ->
            pure ()
    _ ->
      pure ()

substituteInScheme :: Substitution -> Scheme o Kind IndexedType -> IndexedScheme
substituteInScheme sub (Forall _ ts t) = scheme (apply sub ts) (apply sub t)

collectImportedInstances :: (Monad m) => Definition a Kind () -> StateT (ModuleBuild a) (CompilerT a m) [(Path, Name)]
collectImportedInstances =
  \case
    DImport _ (Path ["Builtin$"]) _ -> do
      pure []
    DImport loc path imports -> do
      ModuleBuild{..} <- importedModule loc path
      concatForM imports $
        \case
          -- TODO: cleanup/DRY
          ImportTrait _ name _ ->
            concatForM (Environment.toList moduleInstances) $
              \(trait, is) -> do
                if trait == name
                  then concat <$$> forM (Map.toList is) $
                    \(_, InstanceEntry{..}) -> do
                      forM (Map.toList instanceEntryEntries) $
                        \(f, _) ->
                          pure (path, instanceLabel (Trait trait instanceEntryType) f)
                  else pure []
          ImportType _ name _ ->
            concatForM (Environment.toList moduleInstances) $
              \(trait, is) -> do
                concatForM (Map.toList is) $
                  \(_, InstanceEntry{..}) -> do
                    if headConstructor instanceEntryIndexedType == Just name
                      then forM (Map.toList instanceEntryEntries) $
                        \(f, _) ->
                          pure (path, instanceLabel (Trait trait instanceEntryType) f)
                      else pure []
          _ ->
            pure []
    _ ->
      pure []

collectImportedNames :: (Monad m) => Definition a Kind () -> StateT (ModuleBuild a) (CompilerT a m) ()
collectImportedNames =
  \case
    def@(DImport _ path imports) -> do
      names1 <- collectNameImports def exportedNames
      names2 <- collectTypeImports def exportedTypeNames

      this <- lift $ gets (principalPath . compilerCurrentModule)
      unless (Path ["Builtin$"] == path) $
        forM_ imports $
          \case
            ImportName loc name ->
              unless (name `elem` fmap nameOf names1) $ do
                tellErrors [NameNotInModule name path (ErrorLocation this loc)]
                throwError PreflightFailure
            ImportType loc name _ ->
              unless (name `elem` fmap nameOf names2) $ do
                tellErrors [NameNotInModule name path (ErrorLocation this loc)]
                throwError PreflightFailure
            ImportCotype loc name _ ->
              unless (name `elem` fmap nameOf names2) $ do
                tellErrors [NameNotInModule name path (ErrorLocation this loc)]
                throwError PreflightFailure
            ImportTrait loc name _ ->
              unless (name `elem` fmap nameOf names2) $ do
                tellErrors [NameNotInModule name path (ErrorLocation this loc)]
                throwError PreflightFailure

      forM_ (names1 <> names2) $
        \case
          NFunctionPlaceholder _ ->
            pure ()
          NConstantPlaceholder _ ->
            pure ()
          NFoldPlaceholder _ ->
            pure ()
          NUnfoldPlaceholder _ ->
            pure ()
          info ->
            modify $ addName info
    DQualifiedImport a path -> do
      build <- importedModule a path
      forM_ (exportedNames build <> exportedTypeNames build) $
        \case
          NFunctionPlaceholder _ ->
            pure ()
          NConstantPlaceholder _ ->
            pure ()
          NFoldPlaceholder _ ->
            pure ()
          NUnfoldPlaceholder _ ->
            pure ()
          NFunction name s ->
            modify $ addName (NFunction (qualified name path) s)
          NConstant name s ->
            modify $ addName (NConstant (qualified name path) s)
          NFold name s ->
            modify $ addName (NFold (qualified name path) s)
          NUnfold name s ->
            modify $ addName (NUnfold (qualified name path) s)
          NDataConstructor name s ->
            modify $ addName (NDataConstructor (qualified name path) s)
          NCodataAccessor name s ->
            modify $ addName (NCodataAccessor (qualified name path) s)
          NType name k ->
            modify $ addName (NType (qualified name path) k)
          NCotype name k ->
            modify $ addName (NCotype (qualified name path) k)
          NTrait name ->
            modify $ addName (NTrait (qualified name path))
          NAlias name ->
            modify $ addName (NAlias (qualified name path))
    _ ->
      pure ()

{-# INLINE exportFunction #-}
exportFunction :: (Monad m) => Name -> StateT (ModuleBuild a) (CompilerT a m) ()
exportFunction name = modify $ addName (NFunctionPlaceholder name) . addExport name

{-# INLINE exportConstant #-}
exportConstant :: (Monad m) => Name -> StateT (ModuleBuild a) (CompilerT a m) ()
exportConstant name = modify $ addName (NConstantPlaceholder name) . addExport name

{-# INLINE exportFold #-}
exportFold :: (Monad m) => Name -> StateT (ModuleBuild a) (CompilerT a m) ()
exportFold name = modify $ addName (NFoldPlaceholder name) . addExport name

{-# INLINE exportUnfold #-}
exportUnfold :: (Monad m) => Name -> StateT (ModuleBuild a) (CompilerT a m) ()
exportUnfold name = modify $ addName (NUnfoldPlaceholder name) . addExport name

collectPlaceholders :: (Monad m) => Definition a Kind () -> StateT (ModuleBuild a) (CompilerT a m) ()
collectPlaceholders =
  \case
    DFunction _ name _ _ ->
      exportFunction name
    DConstant _ name _ _ ->
      exportConstant name
    DFold _ name _ ->
      exportFold name
    DUnfold _ name _ ->
      exportUnfold name
    _ ->
      pure ()
