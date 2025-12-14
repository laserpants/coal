{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TranslationPhase.Placeholders (TraitContext (..), passPlaceholders) where

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
import Coal.Language
import Coal.Language.Module
import Coal.TypeSystem.Substitution (Substitutable (apply), Substitution, mapsTo)
import Coal.TypeSystem.Unification
import Control.Monad.Except (MonadError (throwError), forM)
import Control.Monad.Reader (asks, local)
import Control.Monad.State (StateT, execStateT, gets, modify)
import Data.Data (Data)
import Data.Foldable (foldrM)
import Data.Generics.Uniplate.Data (descendM)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Text (isPrefixOf)
import Extras (Dictionary, Name, forM_)

passPlaceholders :: (Monad m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passPlaceholders = Pass{runPass = pass}

pass :: (Monad m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Module Metadata Kind IndexedType)
pass =
  withCurrentModuleC $
    \m -> do
      m1 <- overModuleDefinitionsM (traverse insertPlaceholders) m
      names <- gets compilerNameStore
      updateNames names
      pure m1

updateNames :: (Monad m) => Environment IndexedScheme -> CompilerT a m ()
updateNames store =
  updateBuildC $
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
            NUnfold name _ ->
              go name NUnfold
            NDataConstructor name _ ->
              go name NDataConstructor
            NCodataAccessor name _ ->
              go name NCodataAccessor
            _ ->
              pure ()
 where
  go :: (Monad m) => Name -> (Name -> IndexedScheme -> NameEntry) -> StateT (ModuleBuild a) (CompilerT a m) ()
  go name info =
    case Environment.lookup name store of
      Nothing ->
        pure ()
      Just s ->
        modify $ addName (info name s)

insertPlaceholders :: (Monad m, Monoid a, Data a) => Definition a Kind IndexedType -> CompilerT a m (Definition a Kind IndexedType)
insertPlaceholders =
  \case
    d@(DConstant _ name _ _) ->
      insertTypeInfo name =<< expandInLocalEnv d
    DInstance loc name (InstanceDefinition ts t ds) -> do
      es <- forM ds (insertPlaceholdersInDef (Trait name t))
      pure (DInstance loc name (InstanceDefinition ts t es))
    d@DFold{} ->
      expandInLocalEnv d
    d@DUnfold{} ->
      expandInLocalEnv d
    d ->
      pure d

insertPlaceholdersInDef :: (Monad m, Monoid a, Data a) => Trait ParameterizedType -> Definition a Kind IndexedType -> CompilerT a m (Definition a Kind IndexedType)
insertPlaceholdersInDef trait =
  \case
    c@DConstant{} ->
      insertTypeInfo (instanceLabel trait (definitionName c)) =<< expandInLocalEnv c
    _ ->
      error "Not implemented"

expandInLocalEnv :: (Monad m, TraitContext a b) => b -> CompilerT a m b
expandInLocalEnv d = do
  env1 <- gets compilerNameStore
  local (overCompilerDictionaryNameEnvironment (const env1)) (expandTraits d)

insertTypeInfo :: (Monad m) => Name -> Definition a k IndexedType -> CompilerT a m (Definition a k IndexedType)
insertTypeInfo name d = do
  insertName d name
  pure d

insertName :: (Monad m) => Definition a k IndexedType -> Name -> CompilerT a m ()
insertName (DConstant _ _ (ConstantDefinition _ _ (With ts t) _) _) name = insertNameC name (Forall (typeIndexesIn t) ts t)
insertName _ _ = error "Implementation error"

collectTraits :: (Monad m) => IndexedType -> Name -> CompilerT a m [Trait IndexedType]
collectTraits u name = do
  env <- asks compilerDictionaryNameEnvironment
  case Environment.lookup name env of
    Nothing ->
      pure []
    Just (Forall _ [] _) ->
      pure []
    Just (Forall vs ts t) -> do
      sub1 <- foldrM instantiate mempty vs
      r <- tryMatch (apply sub1 t) u
      case r of
        Left{} ->
          error "TODO"
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

findFirstMatch :: (Monad m) => Trait IndexedType -> CompilerT a m (Maybe (ParameterizedType, IndexedType, Dictionary IndexedScheme))
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
    \(k, InstanceEntry{..}) -> do
      result <- f k
      case result of
        Left{} ->
          pure Nothing
        Right sub ->
          pure $ Just (instanceEntryType, k, Map.map (substituteInScheme sub) instanceEntryEntries)

substituteInScheme :: Substitution -> Scheme o Kind IndexedType -> IndexedScheme
substituteInScheme sub (Forall _ ts t) = scheme (apply sub ts) (apply sub t)

lookupTraitInstance :: (Monoid a, Data a, Monad m) => a -> Trait IndexedType -> CompilerT a m (Maybe (Dictionary (Expression a IndexedType)))
lookupTraitInstance loc trait@(Trait name _) = do
  found <- findFirstMatch trait
  case found of
    Nothing -> do
      if isConcrete trait
        then do
          path <- gets compilerCurrentModule
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

applyTraits :: (Monoid a, Data a, Monad m) => a -> Label IndexedType -> [Trait IndexedType] -> CompilerT a m (Expression a IndexedType)
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
            path <- gets compilerCurrentModule
            tellErrors [MissingInstance trait (ErrorLocation (principalPath path) loc)]
            throwError TraitError
          Nothing -> do
            tellDictionaryTraits [trait]
            pure (ETraitDictionary mempty (typeOf trait) trait)
          Just r ->
            pure (ERecord mempty (typeOf trait) r Nothing)

class TraitContext a d where
  expandTraits :: (Monad m) => d -> CompilerT a m d

expandRecursiveLet :: Expression a IndexedType -> Expression a IndexedType
expandRecursiveLet (ELet a (BPattern _ p e1 :| []) e2) = ERecursiveLet a p e1 e2
expandRecursiveLet _ = error "Implementation error"

instance (Monoid a, Data a) => TraitContext a (Expression a IndexedType) where
  expandTraits =
    \case
      ERecursiveLet a p e1 e2 ->
        expandRecursiveLet <$> expandTraits (ELet a (BPattern a p e1 :| []) e2)
      ELet a bs e -> do
        as <- censorDictionaryTraits (const []) (traverse transformBinding bs)
        let xs = concat (toList (snd <$> as))
        ELet a (fst <$> as) <$> local (overCompilerDictionaryNameEnvironment (Environment.insertMultiple xs)) (expandTraits e)
      EVariable loc (Label t name) -> do
        traits <- collectTraits t name
        applyTraits loc (Label t name) (nub traits)
      ECompiledMatch a t e cs ->
        ECompiledMatch a t <$> expandTraits e <*> traverse expandTraits cs
      EFold a t es cs (Just e) -> do
        e1 <- descendM expandTraits e
        pure (EFold a t es cs (Just e1))
      e ->
        descendM expandTraits e

transformBinding :: (Monoid a, Data a, Monad m) => Binding Expression a IndexedType -> CompilerT a m (Binding Expression a IndexedType, [(Name, IndexedScheme)])
transformBinding =
  \case
    BPattern a var@(PVariable _ (Label _ name)) e
      | "$fold" `isPrefixOf` name -> do
          body <- expandTraits e
          pure (BPattern a var body, [])
    BPattern _ (PVariable a (Label t name)) e -> do
      (e1, traits) <- transformScope e
      let ll = Label (foldTypeOf t (nub traits)) name
      pure (BPattern mempty (PVariable a ll) e1, [(name, Forall (typeIndexesIn t) (nub traits) t)])
    _ ->
      error "Not implemented"

transformScope :: (Monoid a, Data a, Monad m) => Expression a IndexedType -> CompilerT a m (Expression a IndexedType, [Trait IndexedType])
transformScope e = do
  (expr, traits) <- listenDictionaryTraits (expandTraits e)
  case nub traits of
    [] -> pure (expr, traits)
    tr : trs -> pure (dictionaryLambda tr trs expr, traits)

instance (Monoid a, Data a) => TraitContext a (CompiledClause a IndexedType) where
  expandTraits =
    \case
      ECompiledClause a lls e ->
        ECompiledClause a lls <$> expandTraits e

instance (Monoid a, Data a) => TraitContext a (Module a Kind IndexedType) where
  expandTraits =
    overModuleDefinitionsM (traverse expandTraits)

instance (Monoid a, Data a) => TraitContext a (Definition a Kind IndexedType) where
  expandTraits =
    \case
      DConstant loc name c fs ->
        DConstant loc name <$> expandConstantDefinitionTraits name c <*> traverse expandTraits fs
      DFold loc name (FoldDefinition with cs (Just e)) ->
        DFold loc name . FoldDefinition with cs . Just <$> expandTraits e
      DUnfold loc name (UnfoldDefinition with ps d (Just e)) ->
        DUnfold loc name . UnfoldDefinition with ps d . Just <$> expandTraits e
      d ->
        pure d

expandConstantDefinitionTraits :: (Monad m, Monoid a, Data a) => Name -> ConstantDefinition a IndexedType -> CompilerT a m (ConstantDefinition a IndexedType)
expandConstantDefinitionTraits name =
  \case
    ConstantDefinition loc with (With _ t) e -> do
      (expr, traits) <- listenDictionaryTraits (expandTraits e)
      case nub traits of
        [] ->
          pure $ ConstantDefinition loc with (With [] t) expr
        tr : trs -> do
          path <- gets compilerCurrentModule
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
  Expression a (Type o k) ->
  Expression a (Type o k)
dictionaryLambda tr trs = ELambda mempty (dict <$> (tr :| trs))
 where
  dict t = PTraitDictionary mempty (typeOf t) t
