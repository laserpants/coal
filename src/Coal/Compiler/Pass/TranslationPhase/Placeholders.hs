{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TranslationPhase.Placeholders (TraitContext (..), passPlaceholders) where

import Coal.Common.FreeVars
import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Environment (Environment)
import qualified Coal.Common.Environment as Environment
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (supplied)
import Coal.Compiler.Build
import Coal.Compiler.Journal (censorDictionaryTraits, listenDictionaryTraits, tellDictionaryTraits, tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Graphviz.Dot (Dot (..), generateDot, writeDotFile)
import Coal.Language
import Coal.Language.Module
import Coal.ProtoCompiler.ProtoBuild
import Coal.ProtoCompiler.ProtoBuild.ProtoNameEntry
import Coal.ProtoCompiler.ProtoStack
import Coal.ProtoCompiler.ProtoState
import Coal.ProtoLanguage.ProtoModule
import Coal.TypeSystem.Constraint.Assumption (normalizedName)
import Coal.TypeSystem.Substitution (Substitutable (apply), Substitution, mapsTo)
import Coal.TypeSystem.Unification
import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError), forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (asks, local)
import Control.Monad.State (StateT, execStateT, evalStateT, get, gets, modify, put)
import Control.Monad.Trans (lift)
import Data.Data (Data)
import Data.Foldable (foldrM)
import Data.Generics.Uniplate.Data (descendM)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Text (isPrefixOf)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.Lazy (toStrict)
import Debug.Trace
import Extras (Dictionary, Name, forM_, concatForM)
import Text.Pretty.Simple (pPrint, pShowNoColor)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Graph (SCC(..), stronglyConnComp)
import qualified Data.Set as Set
import Data.Foldable.Extra (notNull)
import qualified Data.List.NonEmpty as NonEmpty

passPlaceholders :: (MonadIO m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passPlaceholders = Pass{runPass = pass}

pass :: (MonadIO m) => Module Metadata Kind IndexedType -> CompilerT Metadata (ProtoCompilerT m Metadata) (Module Metadata Kind IndexedType)
pass =
  withCurrentModuleC $
    \m -> do
      lift $ setCurrentPathC (modulePath m)

      -- TODO: This needs some cleanup. We are effectively running the same
      -- steps twice...

      --      _ <- overModuleDefinitionsM (traverse insertPlaceholders) m
      -- names <- gets compilerNameStore
      --      names <- lift $ gets protoOcompilerNameStore
      --      updateNames names
      --      updateNames2 names

      b <- lift protoOgetCurrentBuildC
      lift $ protoOsetNamesC (typeEnvironment b)

----
--
--      baz <- concatForM (moduleDefinitions m) freeNames 
--      let foo = Map.fromList (collapseCycles baz)
--
--      evalStateT considerNext foo 
--
--
----      let foo2 = foo :: Map (Set Name) (Set Name)
--
--      traceShowM ">>>>"
--      traceShowM (modulePath m)
----      traceShowM foo
--
--      --  lift $ protoOsetNamesC env1
--
----

      mm <- overModuleDefinitionsM (traverse insertPlaceholders) m

      -- names2 <- gets compilerNameStore
      names2 <- lift $ gets protoOcompilerNameStore
      -- updateNames names2
      updateNames2 names2

      -- mm2 <- overModuleDefinitionsM (traverse insertPlaceholders) mm

      ----names2 <- gets compilerNameStore
      -- names3 <- lift $ gets protoOcompilerNameStore
      -- updateNames names2
      -- updateNames2 names3

      ProtoBuild{..} <- lift $ protoOgetCurrentBuildC
      liftIO $ Text.writeFile ("tmp/placeholder_names_" <> Text.unpack (principalPath (modulePath mm))) (toStrict $ pShowNoColor $ protoObuildNames)

      liftIO $ Text.writeFile ("tmp/placeholder_build_" <> Text.unpack (principalPath (modulePath mm))) (toStrict $ pShowNoColor $ ProtoBuild{..})

      liftIO $ Text.writeFile ("tmp/placeholder_defs_" <> Text.unpack (principalPath (modulePath mm))) (generateDot mm)

      return mm

-- 
-- leaf => []
-- node => []
-- tree_list_size => [size, tree_list_size]
-- size => [tree_list_size]
-- main => [size, example_tree]
--
--
-- 
-- leaf => []
-- node => []
-- [tree_list_size, size] => [size, tree_list_size]
-- main => [size, example_tree]
--
--
--
--

freeInConstant :: (Show a, Data a) => ConstantDefinition a IndexedType -> Set (Label IndexedType)
freeInConstant ConstantDefinition{..} = freeIn constantDefinitionExpression

freeInFunction :: (Show a, Data a) => FunctionDefinition a IndexedType -> Set (Label IndexedType)
freeInFunction FunctionDefinition{..} = freeSet (boundIn functionDefinitionPatterns) functionDefinitionExpression

freeNames :: (Show a, Monad m, Monoid a, Data a) => Definition a Kind IndexedType -> CompilerT a (ProtoCompilerT m a) [(Name, Set Name)]
freeNames =
  \case
     DConstant _ name def _ ->
       return [(name, Set.map labelName (freeInConstant def))]
     DFunction _ name (def :| _) _ ->
       return [(name, Set.map labelName (freeInFunction def))]
     DInstance _ trait InstanceDefinition{..} ->
       concatForM instanceDefinitionEntries $
         \case
           DConstant _ name def _ ->
             return [(instanceLabel (Trait trait instanceDefinitionType) name, Set.map labelName (freeInConstant def))]
           DFunction _ name (def :| _) _ ->
             return [(instanceLabel (Trait trait instanceDefinitionType) name, Set.map labelName (freeInFunction def))]
     _ ->
       return []

collapseCycles :: [(Name, Set.Set Name)] -> [(Set.Set Name, Set.Set Name)]
collapseCycles defs = map collapseSCC (stronglyConnComp edges)
 where
  edges = [ (name, name, Set.toList deps) | (name, deps) <- defs ]

  collapseSCC :: SCC Name -> (Set.Set Name, Set.Set Name)
  collapseSCC scc = (names, deps `Set.difference` names)
    where
        names =
          case scc of
            AcyclicSCC n -> Set.singleton n
            CyclicSCC ns -> Set.fromList ns

        deps = Set.unions [ lookupDeps n | n <- Set.toList names ]

  lookupDeps n = Map.findWithDefault Set.empty n (Map.fromList defs)

considerNext :: (Monad m) => StateT (Map (Set Name) (Set Name)) (CompilerT a (ProtoCompilerT m a)) ()
considerNext = do
  m <- get
  case Map.keys m of
    [] -> pure ()
    k : _ -> considerKey k
 where
  considerKey :: (Monad m) => Set Name -> StateT (Map (Set Name) (Set Name)) (CompilerT a (ProtoCompilerT m a)) ()
  considerKey key = do
    m <- get
    case Map.lookup key m of
      Nothing ->
        error "Implementation error"
      Just names -> do
        res <- firstNonVisited names
        case res of
          Nothing -> do

            forM_ names $
              \name -> do
                env <- lift $ lift $ gets protoOcompilerNameStore
                case Environment.lookup name env of
                  Nothing ->
                    pure () -- error (show (name, "??"))
                  Just xx ->
                    traceShowM (name, xx)

            modify (Map.delete key)
            considerNext
          Just k ->
            considerKey k

  firstNonVisited :: (Monad m) => Set Name -> StateT (Map (Set Name) (Set Name)) (CompilerT a (ProtoCompilerT m a)) (Maybe (Set Name))
  firstNonVisited names = do
    m <- get
    pure $
      case filter (notNull . Set.intersection names) (Map.keys m) of
        [] -> Nothing
        k : _ -> Just k

--containsAny :: Map (Set Name) (Set Name) -> Set Name -> Maybe (Set Name)
--containsAny m names = 

-- leaf => []
-- node => []
-- [tree_list_size, size] => [size, tree_list_size]
-- main => [size, example_tree]

--  where
--    inKeySets :: Name -> Bool
--    inKeySets name = undefined

updateNames2 :: (Monad m) => Environment IndexedScheme -> CompilerT a (ProtoCompilerT m a) ()
updateNames2 store =
  lift $
    protoOupdateCurrentBuildC $
      \build@ProtoBuild{..} ->
        flip execStateT build $
          forM_ (concat $ Environment.elems protoObuildNames) $
            \case
              ProtoNName name _ ->
                case Environment.lookup (normalizedName name) store of
                  Nothing ->
                    pure ()
                  Just s ->
                    modify (replaceBuildNameEntry (ProtoNName name s))
              _ ->
                pure ()

--updateNames :: (Monad m) => Environment IndexedScheme -> CompilerT a m ()
--updateNames store =
--  updateCurrentBuildC $
--    \build@ModuleBuild{..} ->
--      flip execStateT build $
--        forM_ moduleNames $
--          \case
--            NFunction name _ ->
--              go name NFunction
--            NConstant name _ ->
--              go name NConstant
--            NFold name _ ->
--              go name NFold
--            NDataConstructor name _ ->
--              go name NDataConstructor
--            _ ->
--              pure ()
-- where
--  go :: (Monad m) => Name -> (Name -> IndexedScheme -> NameEntry) -> StateT (ModuleBuild a) (CompilerT a m) ()
--  go name info =
--    case Environment.lookup (normalizedName name) store of
--      Nothing ->
--        pure ()
--      Just s ->
--        modify $ replaceName (info name s)

insertPlaceholders :: (Show a, Monad m, Monoid a, Data a) => Definition a Kind IndexedType -> CompilerT a (ProtoCompilerT m a) (Definition a Kind IndexedType)
insertPlaceholders =
  \case
    d@(DConstant _ name _ _) -> do
      r <- expandInLocalEnv d
      insertTypeInfo name r
    DInstance loc name (InstanceDefinition ts t ds) -> do
      es <- forM ds (insertPlaceholdersInDef (Trait name t))
      pure (DInstance loc name (InstanceDefinition ts t es))
    d ->
      pure d

insertPlaceholdersInDef :: (Show a, Monad m, Monoid a, Data a) => Trait ParameterizedType -> Definition a Kind IndexedType -> CompilerT a (ProtoCompilerT m a) (Definition a Kind IndexedType)
insertPlaceholdersInDef trait =
  \case
    c@DConstant{} -> do
      r <- expandInLocalEnv c
      insertTypeInfo (instanceLabel trait (definitionName c)) r
    _ ->
      error "Not implemented"

expandInLocalEnv :: (Monad m, TraitContext a b) => b -> CompilerT a (ProtoCompilerT m a) b
expandInLocalEnv d = do
  b <- lift protoOgetCurrentBuildC
  let env1 = typeEnvironment b
  expandTraits d

insertTypeInfo :: (Monad m) => Name -> Definition a k IndexedType -> CompilerT a (ProtoCompilerT m a) (Definition a k IndexedType)
insertTypeInfo name d = do
  insertName d name
  pure d

insertName :: (Monad m) => Definition a k IndexedType -> Name -> CompilerT a (ProtoCompilerT m a) ()
insertName (DConstant _ _ (ConstantDefinition _ _ (With ts t) _) _) name = do
  let s = Forall (typeIndexesIn t) (Set.fromList ts) t
  insertNameC name s
  lift $ protoOinsertNameC name s
insertName _ _ = error "Implementation error"

collectTraits :: (Monad m) => IndexedType -> Name -> CompilerT a (ProtoCompilerT m a) (Set (Trait IndexedType))
collectTraits u name = do
  -- env <- asks compilerDictionaryNameEnvironment
  env <- lift $ gets protoOcompilerNameStore
  case Environment.lookup (normalizedName name) env of
    Nothing ->
      pure mempty
    Just (Forall _ s _) | Set.null s ->
      pure mempty
    Just (Forall vs ts t) -> do
      sub1 <- foldrM instantiate mempty vs
      r <- tryMatch (apply sub1 t) u
      case r of
        Left{} ->
          error (show (name, apply sub1 t, u)) -- "TODO"
        Right sub2 ->
          pure (apply (sub2 <> sub1) ts)
 where
  instantiate (TypeIndex k index) acc = do
    var <- supplied (TVariable . TypeIndex k)
    pure (index `mapsTo` var <> acc)

tryMatch :: (Monad m) => IndexedType -> IndexedType -> CompilerT a (ProtoCompilerT m a) (Either UnificationError Substitution)
tryMatch t u = do
  var <- supplied id
  pure (evalUnifier var (match t u))

findFirstMatch :: (Monad m) => Trait IndexedType -> CompilerT a (ProtoCompilerT m a) (Maybe (Type Parameter Kind, IndexedType, Dictionary IndexedScheme))
findFirstMatch (Trait name t) = do
  -- env <- asks compilerInstanceEnvironment
  ProtoBuild{protoObuildInstances} <- lift protoOgetCurrentBuildC
  case Environment.lookup name protoObuildInstances of
    Nothing ->
      pure Nothing
    Just env1 -> do
      kvs <- go (`tryMatch` t) env1
      case kvs of
        [] ->
          pure Nothing
        (t1, k, v) : _ ->
          pure (Just (t1, k, v))
 where
  go f m = fmap catMaybes . forM (Map.toList m) $
    \(k, ProtoInstanceEntry{..}) -> do
      result <- f k
      case result of
        Left{} ->
          pure Nothing
        Right sub ->
          pure $ Just (protoOinstanceEntryType, k, Map.map (substituteInScheme sub) protoOinstanceEntryTypeSchemes)

substituteInScheme :: Substitution -> Scheme o Kind IndexedType -> IndexedScheme
substituteInScheme sub (Forall _ ts t) = scheme (apply sub ts) (apply sub t)

lookupTraitInstance :: (Show a, Monoid a, Data a, Monad m) => a -> Trait IndexedType -> CompilerT a (ProtoCompilerT m a) (Maybe (Dictionary (Expression a () IndexedType)))
lookupTraitInstance loc trait@(Trait name _) = do
  found <- findFirstMatch trait
  case found of
    Nothing -> do
      if isConcrete trait
        then do
          path <- lift $ gets protoOcompilerCurrentPath
          tellErrors [MissingInstance trait (ErrorLocation (principalPath path) loc)]
          throwError TraitError
        else pure Nothing
    Just (t, a, b) ->
      Just <$> Map.traverseWithKey (go t (Trait name a)) b
 where
  go t1 (Trait tn _) n (Forall _ ts t) =
    applyTraits loc (Label t (instanceLabel (Trait tn t1) n)) ts
      >>= expandTraits

isConcrete :: Trait IndexedType -> Bool
isConcrete (Trait _ TIntrinsic{}) = True
isConcrete (Trait _ TRecord{}) = True
isConcrete _ = False

applyTraits :: (Show a, Monoid a, Data a, Monad m) => a -> Label IndexedType -> Set (Trait IndexedType) -> CompilerT a (ProtoCompilerT m a) (Expression a () IndexedType)
applyTraits loc (Label t name) traits =
  if Set.null traits
      then pure (EVariable mempty (Label t name))
      else EApplication mempty t (EVariable mempty (Label t1 name)) <$> traverse insert_ (NonEmpty.fromList (Set.toList traits))
    
--  \case
--    [] ->
--      pure (EVariable mempty (Label t name))
--    tr : trs ->
--      EApplication mempty t (EVariable mempty (Label t1 name)) <$> traverse insert_ (tr :| trs)
     where
      t1 = foldTypeOf t (Set.toList traits) -- (tr : trs)
      insert_ trait = do
        fields <- lookupTraitInstance loc trait
        case fields of
          Nothing | not (isVariable trait) -> do
            path <- lift $ gets protoOcompilerCurrentPath
            -- path <- gets compilerCurrentModule
            tellErrors [MissingInstance trait (ErrorLocation (principalPath path) loc)]
            throwError TraitError
          Nothing -> do
            tellDictionaryTraits (Set.singleton trait)
            pure (ETraitInstance mempty (typeOf trait) trait)
          Just r ->
            pure (ERecord mempty (typeOf trait) r Nothing)

class TraitContext a d where
  expandTraits :: (Monad m) => d -> CompilerT a (ProtoCompilerT m a) d

expandRecursiveLet :: Expression a () IndexedType -> Expression a () IndexedType
expandRecursiveLet (ELet a (BPattern _ p e1 :| []) e2) = ERecursiveLet a p e1 e2
expandRecursiveLet _ = error "Implementation error"

withLocalEnvironment :: (Monad m) => [(Name, IndexedScheme)] -> CompilerT a (ProtoCompilerT m a) r -> CompilerT a (ProtoCompilerT m a) r
withLocalEnvironment xs action = do
  old <- lift get
  lift $ protoOinsertNamesC xs
  r <- action
  lift $ put old
  return r

instance (Monoid a, Data a, Show a) => TraitContext a (Expression a () IndexedType) where
  expandTraits =
    \case
      ERecursiveLet a p e1 e2 ->
        expandRecursiveLet <$> expandTraits (ELet a (BPattern a p e1 :| []) e2)
      ELet a bs e -> do
        as <- censorDictionaryTraits (const mempty) (traverse transformBinding bs)
        let xs = concat (toList (snd <$> as))

        old <- lift get
        lift $ protoOinsertNamesC xs

        r <- ELet a (fst <$> as) <$> expandTraits e

        lift $ put old
        return r
      var@(EVariable _ (Label t name))
        | "$fold" `isPrefixOf` name -> do
            traits <- collectTraits t name
            tellDictionaryTraits traits
            pure var
      EVariable loc (Label t name) -> do
        traits <- collectTraits t name
        applyTraits loc (Label t name) traits
      ECompiledMatch a t e cs ->
        ECompiledMatch a t <$> expandTraits e <*> traverse expandTraits cs
      e ->
        descendM expandTraits e

transformBinding :: (Monoid a, Data a, Show a, Monad m) => Binding Expression a () IndexedType -> CompilerT a (ProtoCompilerT m a) (Binding Expression a () IndexedType, [(Name, IndexedScheme)])
transformBinding =
  \case
    BPattern a var@(PVariable _ (Label t name)) e
      | "$fold" `isPrefixOf` name -> do
          (body, traits) <- listenDictionaryTraits (expandTraits e)
          pure (BPattern a var body, [(name, Forall (typeIndexesIn t) traits t)])
    BPattern _ (PVariable a (Label t name)) e -> do
      (e1, traits) <- transformScope e
      let ll = Label (foldTypeOf t (Set.toList traits)) name
      pure (BPattern mempty (PVariable a ll) e1, [(name, Forall (typeIndexesIn t) traits t)])
    BPattern a (PAnnotation _ _ p) e ->
      transformBinding (BPattern a p e)
    _ ->
      error "Not implemented"

transformScope :: (Monoid a, Data a, Monad m, Show a) => Expression a () IndexedType -> CompilerT a (ProtoCompilerT m a) (Expression a () IndexedType, Set (Trait IndexedType))
transformScope e = do
  (expr, traits) <- listenDictionaryTraits (expandTraits e)
  case Set.toList traits of
    [] -> pure (expr, traits)
    tr : trs -> pure (dictionaryLambda tr trs expr, traits)

instance (Monoid a, Data a, Show a) => TraitContext a (CompiledClause a () IndexedType) where
  expandTraits =
    \case
      ECompiledClause a lls e ->
        ECompiledClause a lls <$> expandTraits e

instance (Monoid a, Data a, Show a) => TraitContext a (Module a Kind IndexedType) where
  expandTraits =
    overModuleDefinitionsM (traverse expandTraits)

instance (Monoid a, Data a, Show a) => TraitContext a (Definition a Kind IndexedType) where
  expandTraits =
    \case
      DConstant loc name c fs ->
        DConstant loc name <$> expandConstantDefinitionTraits name c <*> traverse expandTraits fs
      DInstance loc name (InstanceDefinition ps t ds) ->
        DInstance loc name . InstanceDefinition ps t <$> traverse expandTraits ds
      d ->
        pure d

expandConstantDefinitionTraits :: (Monad m, Monoid a, Data a, Show a) => Name -> ConstantDefinition a IndexedType -> CompilerT a (ProtoCompilerT m a) (ConstantDefinition a IndexedType)
expandConstantDefinitionTraits name =
  \case
    ConstantDefinition loc with (With _ t) e -> do
      (expr, traits) <- listenDictionaryTraits (expandTraits e)
      case Set.toList traits of
        [] ->
          pure $ ConstantDefinition loc with (With [] t) expr
        tr : trs -> do
          -- path <- gets compilerCurrentModule
          path <- lift $ gets protoOcompilerCurrentPath
          -- Insert default int32 instance for Numeric and Ordered traits
          if "main" == name && Path ["Main"] == path
            then do
              recs <- forM (tr :| trs) $
                \(Trait trait _) -> do
                  fields <- fromJust <$> lookupTraitInstance loc (Trait trait (TIntrinsic IInt32))
                  pure $
                    ERecord
                      mempty
                      (applyTypeArgs KTrait (TConstructor (KArrow KType KTrait) trait) (TIntrinsic IInt32 :| []))
                      fields
                      Nothing
              pure $
                ConstantDefinition
                  loc
                  with
                  (With trs t)
                  ( EApplication
                      mempty
                      t
                      (dictionaryLambda tr trs expr)
                      recs
                  )
            else
              pure $
                ConstantDefinition loc with (With (tr : trs) t) (dictionaryLambda tr trs expr)

isVariable :: Trait IndexedType -> Bool
isVariable (Trait _ TVariable{}) = True
isVariable _ = False

dictionaryLambda ::
  (Monoid a, HasType o k (Trait (Type o k))) =>
  Trait (Type o k) ->
  [Trait (Type o k)] ->
  Expression a () (Type o k) ->
  Expression a () (Type o k)
dictionaryLambda tr trs = ELambda mempty (dict <$> (tr :| trs))
 where
  dict t = PTraitInstance mempty (typeOf t) t
