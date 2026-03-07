{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Build.Core (
  buildEnv,
  replacePlaceholders,
  prepareBuild,
  typeConstructorEnv,
  dependencies,
) where

import Coal.AST.Type.Parameterized (instantiateVars)
import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Aliases
import Coal.Compiler.Build
import Coal.Compiler.Builtin.DataConstructors (builtinDataConstructors)
import Coal.Compiler.Builtin.Instances (builtinInstances)
import Coal.Compiler.Embedded (embeddedPaths)
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Stack
import Coal.Compiler.TypeInference (toIndexedScheme, toIndexedType)
import Coal.Language
import Coal.Language.Module
import Coal.ProtoCompiler.ProtoBuild.ProtoNameEntry
import Coal.ProtoCompiler.ProtoStack (ProtoCompilerT (..))
import Coal.TypeSystem.Kind.Inference (inferTraitKinds)
import Coal.TypeSystem.Substitution (Substitutable (apply), Substitution, mapsTo)
import Coal.TypeSystem.Unification (Unifiable (match), evalUnifier)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader
import Control.Monad.State (StateT, evalState, execStateT, get, gets, modify, runStateT)
import Data.Either (rights)
import Data.List (intersect, nub, (\\))
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Extras (Name, for, groupByKey, (<$$>), (<.>))
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
        _ ->
          pure ()

replacePlaceholders :: (Monad m) => Environment IndexedScheme -> CompilerT a m ()
replacePlaceholders store =
  updateCurrentBuildC $
    \build@ModuleBuild{..} -> do
      flip execStateT build $
        forM_ moduleNames $
          \case
            NFunctionPlaceholder name ->
              go name NFunction
            NConstantPlaceholder name ->
              go name NConstant
            NFoldPlaceholder name ->
              go name NFold
            _ ->
              pure ()
 where
  go :: (Monad m) => Name -> (Name -> IndexedScheme -> NameEntry) -> StateT (ModuleBuild a) (CompilerT a m) ()
  go name info =
    case Environment.lookup name store of
      Nothing ->
        pure ()
      -- error "Implementation error"
      Just s ->
        modify $ addName (info name s)

prepareBuild :: (Monad m, Monoid a, Eq a) => Module a Kind () -> CompilerT a (ProtoCompilerT m a) (Module a Kind (), ModuleBuild a)
prepareBuild module_@(Module path exports defs) =
  flip runStateT emptyModuleBuild $ do
    modify (setPath path)

    inEachDef collectTypeConstructors

    -- Built-in type constructors
    modify $
      insertTypeConstructor "List" (TypeConstructorEntry mempty "List" (KArrow KType KType) [])
        . addName (NType "List" (KArrow KType KType))

    aliases <- gets moduleAliases

    kinds <- typeConstructorEnv
    inEachDef (collectDataConstructors aliases kinds)

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
    --    forM_ builtinInstances $
    --      \(name, t, info) ->
    --        modify $ insertInstance name t info

    inEachDef collectImportedNames
    inEachDef collectPlaceholders

    exps <- gets (Set.filter (`notElem` builtin) . moduleExportedNames)
    typeExps <- gets (Set.filter (`notElem` builtin) . moduleTypeExports)

    extra <- nub . concat <$> forM defs collectImportedInstances

    let defs1 = [DImport mempty p (NameImport mempty <$> names) | (p, names) <- groupByKey extra]

    if null exports
      then modify $ setExports (Set.toList exps) . setTypeExports (Set.toList typeExps)
      else do
        let errs =
              flip concatMap exports $
                \case
                  NameExport loc name ->
                    [ExportNotInModule name path (ErrorLocation (principalPath path) loc) | name `notElem` exps]
                  TypeExport loc name _ ->
                    [ExportNotInModule name path (ErrorLocation (principalPath path) loc) | name `notElem` typeExps]
        unless (null errs) $ do
          tellErrors errs
          throwError PreflightFailure

        modify $
          setExports (nameExports exports `intersect` Set.toList exps)
            . setTypeExports (typeExports exports `intersect` Set.toList typeExps)

    env <- lift $ gets compilerVerbatimSource
    case Environment.lookup (principalPath path) env of
      Just src ->
        modify (insertHash src)
      _ ->
        error "Implementation error"

    modify $ setDependencies (filter (`notElem` [Path ["Builtin$"]]) (snd <$> dependencies module_))

    let ds' = defs1 <> defs

    build <- get
    names <- lift $ collectImports build ds'
    modify (setQualifiedNames names)

    return (Module path exports ds')
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
      , "negate"
      ]
  inEachDef = forM_ defs

dependencies :: (Monoid a) => Module a k t -> [(a, Path)]
dependencies (Module p _ defs)
  | principalPath p `elem` embeddedPaths = imported
  | otherwise = imported <> extra
 where
  imported = mapMaybe importPath defs
  extra =
    [ (mempty, Path ["Coal", "Applicative"])
    , (mempty, Path ["Coal", "Monad"])
    ]

nameExports :: [Export a] -> [Name]
nameExports exports =
  flip concatMap exports $
    \case
      NameExport _ name ->
        [name]
      TypeExport _ _ names ->
        names
      _ ->
        []

typeExports :: [Export a] -> [Name]
typeExports exports =
  flip concatMap exports $
    \case
      TypeExport _ name _ ->
        [name]
      _ ->
        []

pick :: (HasName e) => [Name] -> [e] -> [e]
pick names = filter (\e -> nameOf e `Set.member` ns)
 where
  ns = Set.fromList names

collectNameImports :: (Monad m, HasName e) => Definition a Kind () -> (ModuleBuild a -> [e]) -> StateT (ModuleBuild a) (CompilerT a m) [e]
collectNameImports (DImport _ (Path ["Builtin$"]) _) _ = pure []
collectNameImports (DImport loc path imports) getter = do
  build <- lift $ importedModule loc path
  let env = getter build
  pure (pick (nameImports build imports) env)
collectNameImports _ _ = error "Implementation error"

collectTypeImports :: (Monad m, HasName e) => Definition a Kind () -> (ModuleBuild a -> [e]) -> StateT (ModuleBuild a) (CompilerT a m) [e]
collectTypeImports (DImport _ (Path ["Builtin$"]) _) _ = pure []
collectTypeImports (DImport loc path imports) getter = do
  build <- lift $ importedModule loc path
  let env = getter build
  pure (pick (typeImports imports) env)
collectTypeImports _ _ = error "Implementation error"

nameImports :: ModuleBuild a -> [Import a] -> [Name]
nameImports ModuleBuild{..} imports =
  flip concatMap imports $
    \case
      NameImport _ name ->
        [name]
      TypeImport _ name ["*"] ->
        case Environment.lookup name moduleTypeConstructors of
          Just TypeConstructorEntry{..} ->
            typeConstructorEntryDataConstructors
          _ ->
            case Environment.lookup name moduleTraits of
              Just TraitEntry{..} ->
                Environment.names traitEntryEntries
              _ -> []
      TypeImport _ _ names ->
        names

--      TraitImport _ name ["*"] ->
--        case Environment.lookup name moduleTraits of
--          Just TraitEntry{..} ->
--            Environment.names traitEntryEntries
--          _ -> []
--      TraitImport _ _ names ->
--        names

typeImports :: [Import a] -> [Name]
typeImports imports =
  flip concatMap imports $
    \case
      TypeImport _ name _ ->
        [name]
      --      TraitImport _ name _ ->
      --        [name]
      _ ->
        []

collectTypeConstructors :: (Monad m) => Definition a Kind () -> StateT (ModuleBuild a) (CompilerT a m) ()
collectTypeConstructors =
  \case
    DType loc name (TypeDefinition params ctors) -> do
      modify $
        insertTypeConstructor name entry
          . addName (NType name kind)
          . addTypeExport name
     where
      -- TODO: Support higher-kinded type parameters
      kind = foldr KArrow KType (replicate (length params) KType)
      entry = TypeConstructorEntry loc name kind (for ctors constructorName)
    DTypeAlias loc name (AliasDefinition ps t) -> do
      modify $
        insertAlias name entry
          . addName (NAlias name)
          . addTypeExport name
     where
      entry = AliasEntry loc name (parameterName <$> ps) t
    DImport _ (Path ["Builtin$"]) _ ->
      pure ()
    DImport _ path imports -> do
      this <- lift $ gets (principalPath . compilerCurrentModule)
      forM_ imports $
        \case
          TypeImport loc name _ -> do
            found <- insertTypeName path loc name
            unless found $ do
              tellErrors [MissingType name path (ErrorLocation this loc)]
              throwError PreflightFailure
          _ ->
            pure ()
    DQualifiedImport a path -> do
      build <- lift $ importedModule a path
      forM_ (Environment.toList (exportedTypeConstructors build)) $
        \(n, entry) ->
          modify $ insertTypeConstructor (principalPath path <> "." <> n) entry
    _ ->
      pure ()

insertTypeName :: (Monad m) => Path -> a -> Name -> StateT (ModuleBuild a) (CompilerT a m) Bool
insertTypeName path loc name = do
  build <- lift $ importedModule loc path
  case Environment.lookup name (exportedTypeConstructors build) of
    Nothing -> do
      case Environment.lookup name (moduleAliases build) of
        Nothing -> do
          case Environment.lookup name (moduleTraits build) of
            Just TraitEntry{..} -> do
              modify $ insertTrait name TraitEntry{..}
              pure True
            _ ->
              pure False
        Just AliasEntry{..} -> do
          modify $ insertAlias name AliasEntry{..}
          let ts = constructors aliasEntryType
          forM_ ts $ insertTypeName path loc
          pure True
    Just entry -> do
      modify $ insertTypeConstructor name entry
      pure True

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
  gets (foldElems insertTypeInfo . moduleTypeConstructors)
 where
  insertTypeInfo :: TypeConstructorEntry a -> Environment Kind -> Environment Kind
  insertTypeInfo (TypeConstructorEntry _ name kind_ _) = Environment.insert name kind_

collectDataConstructors :: (Monad m) => Environment (AliasEntry a) -> Environment Kind -> Definition a Kind () -> StateT (ModuleBuild a) (CompilerT a (ProtoCompilerT m a)) ()
collectDataConstructors aliases env =
  \case
    DType loc _ def -> do
      entries <- lift $ do
        def' <- local (\e -> e{compilerAliasEnvironment = undefined aliases}) (aliasTransform def)
        dataConstructorEntries env loc def'
      forM_ entries $
        \info@(DataConstructorEntry _ _ DataConstructor{..} _) -> do
          modify $
            addName (NDataConstructor constructorName constructorScheme)
              . insertDataConstructor constructorName info
              . addExport constructorName
    DImport _ (Path ["Builtin$"]) _ ->
      pure ()
    DImport a path imports -> do
      build <- lift $ importedModule a path
      this <- lift $ gets (principalPath . compilerCurrentModule)
      forM_ imports $
        \case
          TypeImport loc name ctors ->
            case Environment.lookup name (exportedTypeConstructors build) of
              Nothing -> do
                case Environment.lookup name (moduleAliases build) of
                  Nothing -> do
                    case Environment.lookup name (moduleTraits build) of
                      Nothing -> do
                        tellErrors [MissingType name path (ErrorLocation this loc)]
                        throwError PreflightFailure
                      Just{} ->
                        pure ()
                  Just{} ->
                    pure ()
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
          _ ->
            pure ()
    DQualifiedImport a path -> do
      build <- lift $ importedModule a path
      forM_ (Environment.toList (exportedDataConstructors build)) $
        \(n, entry) ->
          modify $ insertDataConstructor (principalPath path <> "." <> n) entry
    _ ->
      pure ()

collectTraits :: (Monad m) => Environment Kind -> Definition a Kind () -> StateT (ModuleBuild a) (CompilerT a m) ()
collectTraits env =
  \case
    DTrait loc name def -> do
      case inferTraitKinds env def of
        Left errs -> do
          this <- lift $ gets (principalPath . compilerCurrentModule)
          tellErrors [KindError err (ErrorLocation this loc) | err <- nub errs]
          throwError PreflightFailure
        Right def'@(TraitDefinition ts p ds) -> do
          addTraitEntries loc env name def'
          modify $
            addName (NTrait name)
              . insertTrait name entry
              . addTypeExport name
         where
          entry =
            TraitEntry loc name p ts (Environment.fromList ds)
    DImport _ (Path ["Builtin$"]) _ ->
      pure ()
    def@(DImport loc path _) -> do
      ModuleBuild{..} <- lift $ importedModule loc path
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
      build <- lift $ importedModule a path
      let entries = exportedTraits build
      forM_ entries $
        \entry ->
          modify $ insertTrait (principalPath path <> "." <> traitEntryName entry) entry
    _ ->
      pure ()

addTraitEntries :: (Monad m) => a -> Environment Kind -> Name -> TraitDefinition Kind -> StateT (ModuleBuild a) (CompilerT a m) ()
addTraitEntries loc env trait (TraitDefinition _ p entries) =
  forM_ entries $
    \(name, Forall _ _ t) -> do
      tt <- lift $ toIndexedType loc env p t
      modify $
        addName (NFunction name $ scheme [Trait trait tvar] tt)
          . addExport name
 where
  tvar = TVariable (TypeIndex (parameterKind p) 0)

collectInstances :: (Monad m) => Environment Kind -> Environment (TraitEntry a) -> Definition a Kind () -> StateT (ModuleBuild a) (CompilerT a m) ()
collectInstances kinds traits =
  \case
    DInstance loc trait (InstanceDefinition _ q entries) -> do
      forM_ entries $
        \case
          (DConstant _ name _ _) -> do
            let ll = instanceLabel (Trait trait q) name
            modify $ addName (NFunctionPlaceholder ll)
          (DFunction _ name _ _) -> do
            let ll = instanceLabel (Trait trait q) name
            modify $ addName (NFunctionPlaceholder ll)
          _ ->
            pure ()

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

          t1 <- lift $ toIndexedType loc kinds p q

          Environment env <- flip Environment.mapMEnvironment dict $
            \x -> do
              y <- lift $ toIndexedScheme loc kinds p x
              pure $ substituteInScheme (0 `mapsTo` t1) y

          unless (null extra && null missing) $ do
            forM_ missing $
              \name -> do
                tellErrors [MissingTraitDefinition name trait (ErrorLocation this loc)]
                throwError PreflightFailure
            forM_ extra $
              \name -> do
                tellErrors [UnexpectedTraitDefinition name trait (ErrorLocation this loc)]
                throwError PreflightFailure

          tt <- lift $ toIndexedType loc kinds p q
          modify $ insertInstance trait t1 (InstanceEntry loc q tt env)

          instances <- gets moduleInstances
          forM_ deps $
            \(Trait n t) -> do
              tx <- lift $ toIndexedType loc kinds t q
              case Environment.lookup n instances of
                Nothing -> do
                  tellErrors [MissingRequiredInstance n tx (ErrorLocation this loc)]
                  throwError PreflightFailure
                Just m -> do
                  let keys = Map.keys m
                      i = freshIdIn (tx : keys)
                      res = for keys (evalUnifier i . match tx)

                  when (null (rights res)) $ do
                    tellErrors [MissingRequiredInstance n tx (ErrorLocation this loc)]
                    throwError PreflightFailure
    DImport _ (Path ["Builtin$"]) _ ->
      pure ()
    DImport loc path imports -> do
      ModuleBuild{..} <- lift $ importedModule loc path
      forM_ imports $
        \case
          TypeImport _ name _ ->
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
      ModuleBuild{..} <- lift $ importedModule loc path
      concatForM imports $
        \case
          TypeImport _ name _ ->
            concatForM (Environment.toList moduleInstances) $
              \(trait, is) -> do
                if trait == name
                  then concat <$$> forM (Map.toList is) $
                    \(_, InstanceEntry{..}) -> do
                      forM (Map.toList instanceEntryEntries) $
                        \(f, _) ->
                          pure (path, instanceLabel (Trait trait instanceEntryType) f)
                  else concatForM (Environment.toList moduleInstances) $
                    \(trait, is) -> do
                      concatForM (Map.toList is) $
                        \(_, InstanceEntry{..}) -> do
                          if headConstructor instanceEntryIndexedType == Just name
                            then forM (Map.toList instanceEntryEntries) $
                              \(f, _) -> do
                                let ll = instanceLabel (Trait trait instanceEntryType) f

                                let infos = pick [ll] moduleNames
                                forM_ infos $
                                  \info -> modify $ addName info

                                pure (path, ll)
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
            NameImport loc name ->
              unless (name `elem` fmap nameOf names1) $ do
                tellErrors [ImportNotInModule name path (ErrorLocation this loc)]
                throwError PreflightFailure
            TypeImport loc name _ ->
              unless (name `elem` fmap nameOf names2) $ do
                tellErrors [ImportNotInModule name path (ErrorLocation this loc)]
                throwError PreflightFailure
      --            TraitImport loc name _ ->
      --              unless (name `elem` fmap nameOf names2) $ do
      --                tellErrors [ImportNotInModule name path (ErrorLocation this loc)]
      --                throwError PreflightFailure

      forM_ (names1 <> names2) $
        \case
          NFunctionPlaceholder _ ->
            pure ()
          NConstantPlaceholder _ ->
            pure ()
          NFoldPlaceholder _ ->
            pure ()
          info ->
            modify $ addName info
    DQualifiedImport a path -> do
      build <- lift $ importedModule a path
      forM_ (exportedNames build <> exportedTypeNames build) $
        \case
          NFunctionPlaceholder _ ->
            pure ()
          NConstantPlaceholder _ ->
            pure ()
          NFoldPlaceholder _ ->
            pure ()
          NFunction name s ->
            modify $ addName (NFunction (qualified name path) s)
          NConstant name s ->
            modify $ addName (NConstant (qualified name path) s)
          NFold name s ->
            modify $ addName (NFold (qualified name path) s)
          NDataConstructor name s ->
            modify $ addName (NDataConstructor (qualified name path) s)
          NType name k ->
            modify $ addName (NType (qualified name path) k)
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

collectPlaceholders :: (Monad m) => Definition a Kind () -> StateT (ModuleBuild a) (CompilerT a m) ()
collectPlaceholders =
  \case
    DFunction _ name _ _ ->
      exportFunction name
    DConstant _ name _ _ ->
      exportConstant name
    DFold _ name _ ->
      exportFold name
    _ ->
      pure ()

collectImports :: (Monad m) => ModuleBuild a -> [Definition a k t] -> CompilerT a m (Environment Name)
collectImports build defs = do
  xs <- concatForM defs (qualImports build)
  pure (Environment.fromList xs)

importedModule :: (Monad m) => a -> Path -> CompilerT a m (ModuleBuild a)
importedModule loc path = do
  env <- gets compilerModules
  case Environment.lookup (principalPath path) env of
    Nothing -> do
      this <- gets (principalPath . compilerCurrentModule)
      tellErrors [ModuleNotFound (principalPath path) (ErrorLocation this loc)]
      throwError PreflightFailure
    Just build ->
      return build

qualImports :: (Monad m) => ModuleBuild a -> Definition a k t -> CompilerT a m [(Name, Name)]
qualImports ModuleBuild{..} =
  \case
    DImport _ path names_ ->
      concatForM names_ $
        \case
          NameImport _ name ->
            pure [(name, principalPath path <.> name)]
          TypeImport _ name ["*"] ->
            case Environment.lookup name moduleTypeConstructors of
              Just TypeConstructorEntry{..} ->
                pure [(name_, principalPath path <.> name_) | name_ <- typeConstructorEntryDataConstructors]
              _ ->
                case Environment.lookup name moduleTraits of
                  Just TraitEntry{..} ->
                    pure [(name_, principalPath path <.> name_) | name_ <- Environment.names traitEntryEntries]
                  _ ->
                    pure []
          TypeImport _ _ ctors ->
            pure [(ctor, principalPath path <.> ctor) | ctor <- ctors]
    -- TraitImport _ name ["*"] ->
    --   case Environment.lookup name moduleTraits of
    --     Just TraitEntry{..} ->
    --       pure [(name_, principalPath path <.> name_) | name_ <- Environment.names traitEntryEntries]
    --     _ ->
    --       pure []
    -- TraitImport _ _ entries ->
    --   pure [(entry, principalPath path <.> entry) | entry <- entries]
    DQualifiedImport loc path -> do
      build <- importedModule loc path
      concatForM (exportedNames build) $
        \case
          NFunction name _ ->
            pure [(qualified name path, qualified name path)]
          NConstant name _ ->
            pure [(qualified name path, qualified name path)]
          NFold name _ ->
            pure [(qualified name path, qualified name path)]
          NDataConstructor name _ ->
            pure [(qualified name path, qualified name path)]
          _ ->
            pure []
    _ ->
      pure []

dataConstructorEntries :: (Monad m) => Environment Kind -> a -> TypeDefinition -> CompilerT a m [DataConstructorEntry a]
dataConstructorEntries env loc (TypeDefinition _ ctors) = traverse getEntry ctors
 where
  getEntry DataConstructor{constructorName = name, ..} = do
    sch <- translateScheme loc env constructorScheme
    pure $
      DataConstructorEntry
        loc
        name
        DataConstructor
          { constructorName = name
          , constructorScheme = sch
          , ..
          }
        (Set.fromList (constructorName <$> ctors))

translateScheme :: (Monad m) => a -> Environment Kind -> Scheme Parameter () ParameterizedType -> CompilerT a m IndexedScheme
translateScheme loc env (Forall _ _ s) = do
  case r of
    Left err -> do
      path <- gets compilerCurrentModule
      tellErrors [KindError err (ErrorLocation (principalPath path) loc)]
      throwError PreflightFailure
    Right t -> do
      let vs = typeIndexesIn t
      pure $ Forall vs [] t
 where
  r = evalState (instantiateVars [] env s) (0 :: Int)
