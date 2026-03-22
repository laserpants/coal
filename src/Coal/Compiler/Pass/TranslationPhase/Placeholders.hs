{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TranslationPhase.Placeholders (TraitContext (..), passPlaceholders) where

import Coal.AST.HasMetadata (HasMetadata (..))
import Coal.Compiler.TypeInference.Errors (prettyErrorMessage)
import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Environment (Environment)
import qualified Coal.Common.Environment as Environment
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (supplied)
import Coal.Compiler.Build
import Coal.Compiler.Environment (overCompilerDictionaryNameEnvironment)
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
import Control.Monad.State (StateT, execStateT, get, gets, modify, put)
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
import Extras (Dictionary, Name, forM_)
import Text.Pretty.Simple (pPrint, pShowNoColor)

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

      --  lift $ protoOsetNamesC env1

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

updateNames2 :: (Monad m) => Environment IndexedScheme -> CompilerT a (ProtoCompilerT m Metadata) ()
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

updateNames :: (Monad m) => Environment IndexedScheme -> CompilerT a m ()
updateNames store =
  updateCurrentBuildC $
    \build@ModuleBuild{..} ->
      flip execStateT build $
        forM_ moduleNames $
          \case
            NFunction name _ ->
              go name NFunction
            NConstant name _ ->
              go name NConstant
            NFold name _ ->
              go name NFold
            NDataConstructor name _ ->
              go name NDataConstructor
            _ ->
              pure ()
 where
  go :: (Monad m) => Name -> (Name -> IndexedScheme -> NameEntry) -> StateT (ModuleBuild a) (CompilerT a m) ()
  go name info =
    case Environment.lookup (normalizedName name) store of
      Nothing ->
        pure ()
      Just s ->
        modify $ replaceName (info name s)

insertPlaceholders :: (HasMetadata a, Show a, Monad m, Monoid a, Data a) => Definition a Kind IndexedType -> CompilerT a (ProtoCompilerT m Metadata) (Definition a Kind IndexedType)
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

insertPlaceholdersInDef :: (HasMetadata a, Show a, Monad m, Monoid a, Data a) => Trait ParameterizedType -> Definition a Kind IndexedType -> CompilerT a (ProtoCompilerT m Metadata) (Definition a Kind IndexedType)
insertPlaceholdersInDef trait =
  \case
    c@DConstant{} -> do
      r <- expandInLocalEnv c
      insertTypeInfo (instanceLabel trait (definitionName c)) r
    _ ->
      error "Not implemented"

expandInLocalEnv :: (Monad m, TraitContext a b) => b -> CompilerT a (ProtoCompilerT m Metadata) b
expandInLocalEnv d = do
  b <- lift protoOgetCurrentBuildC
  let env1 = typeEnvironment b
  local (overCompilerDictionaryNameEnvironment (const env1)) (expandTraits d)

insertTypeInfo :: (Monad m) => Name -> Definition a k IndexedType -> CompilerT a (ProtoCompilerT m Metadata) (Definition a k IndexedType)
insertTypeInfo name d = do
  insertName d name
  pure d

insertName :: (Monad m) => Definition a k IndexedType -> Name -> CompilerT a (ProtoCompilerT m Metadata) ()
insertName (DConstant _ _ (ConstantDefinition _ _ (With ts t) _) _) name = do
  let s = Forall (typeIndexesIn t) ts t
  insertNameC name s
  lift $ protoOinsertNameC name s
insertName _ _ = error "Implementation error"

collectTraits :: (Monad m) => IndexedType -> Name -> CompilerT a (ProtoCompilerT m Metadata) [Trait IndexedType]
collectTraits u name = do
  -- env <- asks compilerDictionaryNameEnvironment
  env <- lift $ gets protoOcompilerNameStore
  case Environment.lookup (normalizedName name) env of
    Nothing ->
      pure []
    Just (Forall _ [] _) ->
      pure []
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

tryMatch :: (Monad m) => IndexedType -> IndexedType -> CompilerT a m (Either UnificationError Substitution)
tryMatch t u = do
  var <- supplied id
  pure (evalUnifier var (match t u))

findFirstMatch :: (Monad m) => Trait IndexedType -> CompilerT a m (Maybe (Type Parameter Kind, IndexedType, Dictionary IndexedScheme))
findFirstMatch (Trait name t) = do
  env <- asks compilerInstanceEnvironment
  case Environment.lookup name env of
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

lookupTraitInstance :: (HasMetadata a, Show a, Monoid a, Data a, Monad m) => a -> Trait IndexedType -> CompilerT a (ProtoCompilerT m Metadata) (Maybe (Dictionary (Expression a () IndexedType)))
lookupTraitInstance loc trait@(Trait name _) = do
  found <- findFirstMatch trait
  case found of
    Nothing -> do
      if isConcrete trait
        then do
          path <- lift $ gets protoOcompilerCurrentPath

          --traceShowM (prettyErrorMessage ["here"] loc "")

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

applyTraits :: (HasMetadata a, Show a, Monoid a, Data a, Monad m) => a -> Label IndexedType -> [Trait IndexedType] -> CompilerT a (ProtoCompilerT m Metadata) (Expression a () IndexedType)
applyTraits loc (Label t name) =
  \case
    [] ->
      pure (EVariable mempty (Label t name))
    tr : trs ->
      EApplication mempty t (EVariable mempty (Label t1 name)) <$> traverse insert_ (tr :| trs)
     where
      t1 = foldTypeOf t (tr : trs)
      insert_ trait = do
        fields <- lookupTraitInstance loc trait
        case fields of
          Nothing | not (isVariable trait) -> do
            path <- lift $ gets protoOcompilerCurrentPath
            -- path <- gets compilerCurrentModule
            tellErrors [MissingInstance trait (ErrorLocation (principalPath path) loc)]
            throwError TraitError
          Nothing -> do
            tellDictionaryTraits [trait]
            pure (ETraitInstance mempty (typeOf trait) trait)
          Just r -> do
            --traceShowM r
            sub <- lift $ gets protoOcompilerSubstitution
            pure (ERecord mempty (typeOf trait) (apply sub r) Nothing)

class TraitContext a d where
  expandTraits :: (Monad m) => d -> CompilerT a (ProtoCompilerT m Metadata) d

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

instance (HasMetadata a, Monoid a, Data a, Show a) => TraitContext a (Expression a () IndexedType) where
  expandTraits =
    \case
      ERecursiveLet a p e1 e2 ->
        expandRecursiveLet <$> expandTraits (ELet a (BPattern a p e1 :| []) e2)
      ELet a bs e -> do
        as <- censorDictionaryTraits (const []) (traverse transformBinding bs)
        let xs = concat (toList (snd <$> as))

        old <- lift get
        lift $ protoOinsertNamesC xs

        r <- ELet a (fst <$> as) <$> local (overCompilerDictionaryNameEnvironment (Environment.insertMultiple xs)) (expandTraits e)

        lift $ put old
        return r
      var@(EVariable _ (Label t name))
        | "$fold" `isPrefixOf` name -> do
            traits <- collectTraits t name
            tellDictionaryTraits traits
            pure var
      EVariable loc (Label t name) -> do
        traits <- collectTraits t name

        when ("and_eval" == name) $
          traceShowM traits
        
        applyTraits loc (Label t name) (nub traits)
      ECompiledMatch a t e cs ->
        ECompiledMatch a t <$> expandTraits e <*> traverse expandTraits cs
      e ->
        descendM expandTraits e

transformBinding :: (HasMetadata a, Monoid a, Data a, Show a, Monad m) => Binding Expression a () IndexedType -> CompilerT a (ProtoCompilerT m Metadata) (Binding Expression a () IndexedType, [(Name, IndexedScheme)])
transformBinding =
  \case
    BPattern a var@(PVariable _ (Label t name)) e
      | "$fold" `isPrefixOf` name -> do
          (body, traits) <- listenDictionaryTraits (expandTraits e)
          pure (BPattern a var body, [(name, Forall (typeIndexesIn t) (nub traits) t)])
    BPattern _ (PVariable a (Label t name)) e -> do
      (e1, traits) <- transformScope e
      let ll = Label (foldTypeOf t (nub traits)) name
      pure (BPattern mempty (PVariable a ll) e1, [(name, Forall (typeIndexesIn t) (nub traits) t)])
    BPattern a (PAnnotation _ _ p) e ->
      transformBinding (BPattern a p e)
    _ ->
      error "Not implemented"

transformScope :: (HasMetadata a, Monoid a, Data a, Monad m, Show a) => Expression a () IndexedType -> CompilerT a (ProtoCompilerT m Metadata) (Expression a () IndexedType, [Trait IndexedType])
transformScope e = do
  (expr, traits) <- listenDictionaryTraits (expandTraits e)
  case nub traits of
    [] -> pure (expr, traits)
    tr : trs -> pure (dictionaryLambda tr trs expr, traits)

instance (HasMetadata a, Monoid a, Data a, Show a) => TraitContext a (CompiledClause a () IndexedType) where
  expandTraits =
    \case
      ECompiledClause a lls e ->
        ECompiledClause a lls <$> expandTraits e

instance (HasMetadata a, Monoid a, Data a, Show a) => TraitContext a (Module a Kind IndexedType) where
  expandTraits =
    overModuleDefinitionsM (traverse expandTraits)

instance (HasMetadata a, Monoid a, Data a, Show a) => TraitContext a (Definition a Kind IndexedType) where
  expandTraits =
    \case
      DConstant loc name c fs ->
        DConstant loc name <$> expandConstantDefinitionTraits name c <*> traverse expandTraits fs
      d ->
        pure d

expandConstantDefinitionTraits :: (HasMetadata a, Monad m, Monoid a, Data a, Show a) => Name -> ConstantDefinition a IndexedType -> CompilerT a (ProtoCompilerT m Metadata) (ConstantDefinition a IndexedType)
expandConstantDefinitionTraits name =
  \case
    ConstantDefinition loc with (With _ t) e -> do
      (expr, traits) <- listenDictionaryTraits (expandTraits e)
      case nub traits of
        [] ->
          pure $ ConstantDefinition loc with (With [] t) expr
        tr : trs -> do
          -- path <- gets compilerCurrentModule
          path <- lift $ gets protoOcompilerCurrentPath
          -- Insert default int32 instance for Numeric and Ordered traits
          --if "main" == name && Path ["Main"] == path
          --  then do
          --    recs <- forM (tr :| trs) $
          --      \(Trait trait _) -> do
          --        traceShowM (tr :| trs) 
          --        fields <- fromJust <$> lookupTraitInstance loc (Trait trait (TIntrinsic IInt32))
          --        pure $
          --          ERecord
          --            mempty
          --            (applyTypeArgs KTrait (TConstructor (KArrow KType KTrait) trait) (TIntrinsic IInt32 :| []))
          --            fields
          --            Nothing
          --    pure $
          --      ConstantDefinition
          --        loc
          --        with
          --        (With trs t)
          --        ( EApplication
          --            mempty
          --            t
          --            (dictionaryLambda tr trs expr)
          --            recs
          --        )
          --  else
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
