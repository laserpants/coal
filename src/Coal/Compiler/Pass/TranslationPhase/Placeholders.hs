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

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Environment (Environment)
import qualified Coal.Common.Environment as Environment
import Coal.Common.FreeVars
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (supplied)
import Coal.Compiler.Build
import Coal.Compiler.Build.NameEntry
import Coal.Compiler.Journal (censorDictionaryTraits, listenDictionaryTraits, tellDictionaryTraits, tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Compiler.State
import Coal.Language
import Coal.Language.Definition
import Coal.Language.Module
import Coal.Language.Module.Path

-- import Coal.TypeSystem.Constraint.Assumption (normalizedName)
import Coal.TypeSystem.Substitution (Substitutable (apply), Substitution, mapsTo)
import Coal.TypeSystem.Unification
import Control.Monad (replicateM_, when)
import Control.Monad.Except (MonadError (throwError), forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (asks, local)
import Control.Monad.State (StateT, evalStateT, execStateT, get, gets, modify, put)
import Control.Monad.Trans (lift)
import Data.Data (Data)
import Data.Foldable (foldrM)
import Data.Foldable.Extra (notNull)
import Data.Generics.Uniplate.Data (descendM)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (isPrefixOf)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.Lazy (toStrict)
import Debug.Trace
import Extras (Dictionary, Name, concatForM, forM_, traverse_, twice)
import Text.Pretty.Simple (pPrint, pShowNoColor)

passPlaceholders :: (MonadIO m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passPlaceholders = Pass{runPass = pass}

pass :: (MonadIO m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Module Metadata Kind IndexedType)
pass m = do
  --  withCurrentModuleC $
  --    \m -> do
  setCurrentPathC (modulePath m)

  b <- getCurrentBuildC
  setNamesC (typeEnvironment b)

  twice (traverse_ cafe2 (moduleDefinitions m))
  --  traverse_ cafe2 (moduleDefinitions m)

  names <- gets compilerNameStore
  updateNames names

  --      Build{..} <- lift $ getCurrentBuildC
  --      liftIO $ Text.writeFile ("tmp/placeholder_1names_" <> Text.unpack (principalPath (modulePath m))) (toStrict $ pShowNoColor $ buildNames)
  --      liftIO $ Text.writeFile ("tmp/placeholder_1build_" <> Text.unpack (principalPath (modulePath m))) (toStrict $ pShowNoColor $ Build{..})
  --      liftIO $ Text.writeFile ("tmp/placeholder_1defs_" <> Text.unpack (principalPath (modulePath m))) (generateDot m)

  -- mm <- overModuleDefinitionsM (traverse insertPlaceholders) m

  mm <- traverse insertPlaceholders2 (moduleDefinitions m)

  --      Build{..} <- lift $ getCurrentBuildC
  --      liftIO $ Text.writeFile ("tmp/placeholder_names_" <> Text.unpack (principalPath (modulePath mm))) (toStrict $ pShowNoColor $ buildNames)
  --      liftIO $ Text.writeFile ("tmp/placeholder_build_" <> Text.unpack (principalPath (modulePath mm))) (toStrict $ pShowNoColor $ Build{..})
  --      liftIO $ Text.writeFile ("tmp/placeholder_defs_" <> Text.unpack (principalPath (modulePath mm))) (generateDot mm)

  return m{moduleDefinitions = mm}

-- TODO: Move

updateNames :: (Monad m) => Environment IndexedScheme -> CompilerT a m ()
updateNames store =
  updateCurrentBuildC $
    \build@Build{..} ->
      flip execStateT build $
        forM_ (concat $ Environment.elems buildNames) $
          \case
            NName name _ ->
              case Environment.lookup name store of
                Nothing ->
                  pure ()
                Just s ->
                  modify (replaceBuildNameEntry (NName name s))
            _ ->
              pure ()

insertPlaceholders2 :: (Show a, Monad m, Monoid a, Data a) => Definition a Kind IndexedType -> CompilerT a m (Definition a Kind IndexedType)
insertPlaceholders2 =
  \case
    def@DLet{} ->
      expandTraits def
    DInstance loc InstanceDefinition{..} -> do
      newInstanceDefinitionImplementations <- forM instanceDefinitionImplementations insertPlaceholdersInDef2
      return $
        DInstance
          loc
          InstanceDefinition
            { instanceDefinitionImplementations = newInstanceDefinitionImplementations
            , ..
            }
    d ->
      pure d

-- insertPlaceholders :: (Show a, Monad m, Monoid a, Data a) => Definition a Kind IndexedType -> CompilerT a m (Definition a Kind IndexedType)
-- insertPlaceholders =
--  \case
--    d@(DConstant _ name _ _) -> do
--      undefined -- expandTraits d
--    DInstance loc name (InstanceDefinition ts t ds) -> do
--      es <- forM ds insertPlaceholdersInDef
--      pure (DInstance loc name (InstanceDefinition ts t es))
--    d ->
--      pure d

-- insertPlaceholdersInDef2 :: (Show a, Monad m, Monoid a, Data a) => Definition a Kind IndexedType -> CompilerT a m (Definition a Kind IndexedType)
-- insertPlaceholdersInDef2 =
--  \case
----    c@DConstant{} -> do
----      expandTraits c
--    _ ->
--      error "Not implemented"

-- insertPlaceholdersInDef :: (Show a, Monad m, Monoid a, Data a) => Definition a Kind IndexedType -> CompilerT a m (Definition a Kind IndexedType)
-- insertPlaceholdersInDef =
--  \case
--    c@DConstant{} -> do
--      expandTraits c
--    _ ->
--      error "Not implemented"

insertPlaceholdersInDef2 :: (Show a, Monad m, Monoid a, Data a) => Definition a Kind IndexedType -> CompilerT a m (Definition a Kind IndexedType)
insertPlaceholdersInDef2 =
  \case
    def@DLet{} -> do
      expandTraits def
    d ->
      pure d

-- insertName :: (Monad m) => Definition a k IndexedType -> Name -> CompilerT a m ()
-- insertName (DConstant _ _ (ConstantDefinition _ _ (With ts t) _) _) name = do
--  let s = Forall (typeIndexesIn t) (Set.fromList ts) t
--  lift $ insertNameC name s
-- insertName _ _ = error "Implementation error"

insertName2 :: (Monad m) => Definition a k IndexedType -> Name -> CompilerT a m ()
insertName2 (DLet _ _ (LetDefinition _ _ (With ts t) _)) name = do
  let s = Forall (typeIndexesIn t) (Set.fromList ts) t
  insertNameC name s
insertName2 _ _ = error "Implementation error"

collectTraits :: (Monad m) => IndexedType -> Name -> CompilerT a m (Set (Trait IndexedType))
collectTraits u name = do
  env <- gets compilerNameStore
  case Environment.lookup name env of
    Nothing ->
      pure mempty
    Just (Forall _ s _)
      | Set.null s ->
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

tryMatch :: (Monad m) => IndexedType -> IndexedType -> CompilerT a m (Either UnificationError Substitution)
tryMatch t u = do
  var <- supplied id
  pure (evalUnifier var (match t u))

findFirstMatch :: (Monad m) => Trait IndexedType -> CompilerT a m (Maybe (Type Parameter Kind, IndexedType, Dictionary IndexedScheme))
findFirstMatch (Trait name t) = do
  Build{buildInstances} <- getCurrentBuildC
  case Environment.lookup name buildInstances of
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
          pure $ Just (instanceEntryType, k, Map.map (substituteInScheme sub) instanceEntryTypeSchemes)

substituteInScheme :: Substitution -> Scheme o Kind IndexedType -> IndexedScheme
substituteInScheme sub (Forall _ ts t) = scheme (apply sub ts) (apply sub t)

lookupTraitInstance2 :: (Show a, Monoid a, Data a, Data k, Monad m) => a -> Trait IndexedType -> CompilerT a m (Maybe (Dictionary (Expression a k IndexedType)))
lookupTraitInstance2 loc trait@(Trait name _) = do
  found <- findFirstMatch trait
  case found of
    Nothing -> do
      if isConcrete trait
        then do
          path <- gets compilerCurrentPath
          tellErrors [MissingInstance trait (ErrorLocation (principalPath path) loc)]
          throwError TraitError
        else pure Nothing
    Just (t, a, b) ->
      Just <$> Map.traverseWithKey (go t (Trait name a)) b
 where
  go t1 (Trait tn _) n (Forall _ ts t) =
    applyTraits loc (Label t (instanceLabel (Trait tn t1) n)) ts
      >>= expandTraits

-- lookupTraitInstance :: (Show a, Monoid a, Data a, Monad m) => a -> Trait IndexedType -> CompilerT a m (Maybe (Dictionary (Expression a k IndexedType)))
-- lookupTraitInstance loc trait@(Trait name _) = do
--  found <- findFirstMatch trait
--  case found of
--    Nothing -> do
--      if isConcrete trait
--        then do
--          path <- lift $ gets compilerCurrentPath
--          tellErrors [MissingInstance trait (ErrorLocation (principalPath path) loc)]
--          throwError TraitError
--        else pure Nothing
--    Just (t, a, b) ->
--      Just <$> Map.traverseWithKey (go t (Trait name a)) b
-- where
--  go t1 (Trait tn _) n (Forall _ ts t) =
--    applyTraits loc (Label t (instanceLabel (Trait tn t1) n)) ts
--      >>= expandTraits

isConcrete :: Trait IndexedType -> Bool
isConcrete (Trait _ TIntrinsic{}) = True
isConcrete (Trait _ TRecord{}) = True
isConcrete _ = False

applyTraits :: (Show a, Monoid a, Data a, Data k, Monad m) => a -> Label IndexedType -> Set (Trait IndexedType) -> CompilerT a m (Expression a k IndexedType)
applyTraits loc (Label t name) traits =
  if Set.null traits
    then pure (EVariable mempty (Label t name))
    else EApplication mempty t (EVariable mempty (Label t1 name)) <$> traverse insert_ (NonEmpty.fromList (Set.toList traits))
 where
  t1 = foldTypeOf t (Set.toList traits) -- (tr : trs)
  insert_ trait = do
    fields <- lookupTraitInstance2 loc trait
    case fields of
      Nothing | not (isVariable trait) -> do
        path <- gets compilerCurrentPath
        tellErrors [MissingInstance trait (ErrorLocation (principalPath path) loc)]
        throwError TraitError
      Nothing -> do
        tellDictionaryTraits (Set.singleton trait)
        pure (ETraitInstance mempty (typeOf trait) trait)
      Just r ->
        pure (ERecord mempty (typeOf trait) r Nothing)

class TraitContext a d where
  expandTraits :: (Monad m) => d -> CompilerT a m d

expandRecursiveLet :: Expression a k IndexedType -> Expression a k IndexedType
expandRecursiveLet (ELet a (BPattern _ p e1 :| []) e2) = ERecursiveLet a p e1 e2
expandRecursiveLet _ = error "Implementation error"

withLocalEnvironment :: (Monad m) => [(Name, IndexedScheme)] -> CompilerT a m r -> CompilerT a m r
withLocalEnvironment xs action = do
  old <- get
  insertNamesC xs
  r <- action
  put old
  return r

instance (Monoid a, Data a, Data k, Show a) => TraitContext a (Expression a k IndexedType) where
  expandTraits =
    \case
      ERecursiveLet a p e1 e2 ->
        expandRecursiveLet <$> expandTraits (ELet a (BPattern a p e1 :| []) e2)
      ELet a bs e -> do
        as <- censorDictionaryTraits (const mempty) (traverse transformBinding2 bs)
        let xs = concat (toList (snd <$> as))

        old <- get
        insertNamesC xs

        r <- ELet a (fst <$> as) <$> expandTraits e

        put old
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

-- instance (Monoid a, Data a, Show a) => TraitContext a (Expression a () IndexedType) where
--  expandTraits =
--    \case
--      ERecursiveLet a p e1 e2 ->
--        expandRecursiveLet <$> expandTraits (ELet a (BPattern a p e1 :| []) e2)
--      ELet a bs e -> do
--        as <- censorDictionaryTraits (const mempty) (traverse transformBinding bs)
--        let xs = concat (toList (snd <$> as))
--
--        old <- lift get
--        lift $ insertNamesC xs
--
--        r <- ELet a (fst <$> as) <$> expandTraits e
--
--        lift $ put old
--        return r
--      var@(EVariable _ (Label t name))
--        | "$fold" `isPrefixOf` name -> do
--            traits <- collectTraits t name
--            tellDictionaryTraits traits
--            pure var
--      EVariable loc (Label t name) -> do
--        traits <- collectTraits t name
--        applyTraits loc (Label t name) traits
--      ECompiledMatch a t e cs ->
--        ECompiledMatch a t <$> expandTraits e <*> traverse expandTraits cs
--      e ->
--        descendM expandTraits e

transformBinding2 :: (Monoid a, Data a, Data k, Show a, Monad m) => Binding Expression a k IndexedType -> CompilerT a m (Binding Expression a k IndexedType, [(Name, IndexedScheme)])
transformBinding2 =
  \case
    BPattern a var@(PVariable _ (Label t name)) e
      | "$fold" `isPrefixOf` name -> do
          (body, traits) <- listenDictionaryTraits (expandTraits e)
          pure (BPattern a var body, [(name, Forall (typeIndexesIn t) traits t)])
    BPattern _ (PVariable a (Label t name)) e -> do
      (e1, traits) <- transformScope2 e
      let ll = Label (foldTypeOf t (Set.toList traits)) name
      pure (BPattern mempty (PVariable a ll) e1, [(name, Forall (typeIndexesIn t) traits t)])
    BPattern a (PAnnotation _ _ p) e ->
      transformBinding2 (BPattern a p e)
    _ ->
      error "Not implemented"

-- transformBinding :: (Monoid a, Data a, Show a, Monad m) => Binding Expression a () IndexedType -> CompilerT a m (Binding Expression a () IndexedType, [(Name, IndexedScheme)])
-- transformBinding =
--  \case
--    BPattern a var@(PVariable _ (Label t name)) e
--      | "$fold" `isPrefixOf` name -> do
--          (body, traits) <- listenDictionaryTraits (expandTraits e)
--          pure (BPattern a var body, [(name, Forall (typeIndexesIn t) traits t)])
--    BPattern _ (PVariable a (Label t name)) e -> do
--      (e1, traits) <- transformScope e
--      let ll = Label (foldTypeOf t (Set.toList traits)) name
--      pure (BPattern mempty (PVariable a ll) e1, [(name, Forall (typeIndexesIn t) traits t)])
--    BPattern a (PAnnotation _ _ p) e ->
--      transformBinding (BPattern a p e)
--    _ ->
--      error "Not implemented"

transformScope2 :: (Monoid a, Data a, Data k, Monad m, Show a) => Expression a k IndexedType -> CompilerT a m (Expression a k IndexedType, Set (Trait IndexedType))
transformScope2 e = do
  (expr, traits) <- listenDictionaryTraits (expandTraits e)
  case Set.toList traits of
    [] -> pure (expr, traits)
    tr : trs -> pure (dictionaryLambda tr trs expr, traits)

-- transformScope :: (Monoid a, Data a, Monad m, Show a) => Expression a () IndexedType -> CompilerT a m (Expression a () IndexedType, Set (Trait IndexedType))
-- transformScope e = do
--  (expr, traits) <- listenDictionaryTraits (expandTraits e)
--  case Set.toList traits of
--    [] -> pure (expr, traits)
--    tr : trs -> pure (dictionaryLambda tr trs expr, traits)

instance (Monoid a, Data a, Data k, Show a) => TraitContext a (CompiledClause a k IndexedType) where
  expandTraits =
    \case
      ECompiledClause a lls e ->
        ECompiledClause a lls <$> expandTraits e

-- instance (Monoid a, Data a, Show a) => TraitContext a (Module a k IndexedType) where
--  expandTraits = undefined
--    overModuleDefinitionsM (traverse expandTraits)

instance (Monoid a, Data a, Data k, Show a) => TraitContext a (Definition a k IndexedType) where
  expandTraits =
    \case
      DLet loc name letDefinition -> do
        newLetDefinition <- expandLetDefinitionTraits name letDefinition
        return $ DLet loc name newLetDefinition
      DInstance loc InstanceDefinition{..} -> do
        newInstanceDefinitionImplementations <- traverse expandTraits instanceDefinitionImplementations
        return $
          DInstance
            loc
            InstanceDefinition
              { instanceDefinitionImplementations = newInstanceDefinitionImplementations
              , ..
              }
      d ->
        return d

-- instance (Monoid a, Data a, Show a) => TraitContext a (Definition a Kind IndexedType) where
--  expandTraits =
--    undefined
--    \case
--      DConstant loc name c fs ->
--        DConstant loc name <$> expandConstantDefinitionTraits name c <*> traverse expandTraits fs
--      DInstance loc name (InstanceDefinition ps t ds) ->
--        DInstance loc name . InstanceDefinition ps t <$> traverse expandTraits ds
--      d ->
--        pure d

expandLetDefinitionTraits :: (Monad m, Monoid a, Data a, Data k, Show a) => Name -> LetDefinition a k IndexedType -> CompilerT a m (LetDefinition a k IndexedType)
expandLetDefinitionTraits name =
  \case
    LetDefinition loc with (With _ t) e -> do
      (expr, traits) <- listenDictionaryTraits (expandTraits e)
      case Set.toList traits of
        [] ->
          pure $ LetDefinition loc with (With [] t) expr
        tr : trs -> do
          -- path <- gets compilerCurrentModule
          path <- gets compilerCurrentPath
          -- Insert default int32 instance for Numeric and Ordered traits
          if "main" == name && Path ["Main"] == path
            then do
              recs <- forM (tr :| trs) $
                \(Trait trait _) -> do
                  fields <- fromJust <$> lookupTraitInstance2 loc (Trait trait (TIntrinsic IInt32))
                  pure $
                    ERecord
                      mempty
                      (applyTypeArgs KTrait (TConstructor (KArrow KType KTrait) trait) (TIntrinsic IInt32 :| []))
                      fields
                      Nothing
              pure $
                LetDefinition
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
                LetDefinition loc with (With (tr : trs) t) (dictionaryLambda tr trs expr)

-- expandConstantDefinitionTraits :: (Monad m, Monoid a, Data a, Show a) => Name -> ConstantDefinition a IndexedType -> CompilerT a m (ConstantDefinition a IndexedType)
-- expandConstantDefinitionTraits name =
--  \case
--    ConstantDefinition loc with (With _ t) e -> do
--      (expr, traits) <- listenDictionaryTraits (expandTraits e)
--      case Set.toList traits of
--        [] ->
--          pure $ ConstantDefinition loc with (With [] t) expr
--        tr : trs -> do
--          -- path <- gets compilerCurrentModule
--          path <- lift $ gets compilerCurrentPath
--          -- Insert default int32 instance for Numeric and Ordered traits
--          if "main" == name && Path ["Main"] == path
--            then do
--              recs <- forM (tr :| trs) $
--                \(Trait trait _) -> do
--                  fields <- fromJust <$> lookupTraitInstance loc (Trait trait (TIntrinsic IInt32))
--                  pure $
--                    ERecord
--                      mempty
--                      (applyTypeArgs KTrait (TConstructor (KArrow KType KTrait) trait) (TIntrinsic IInt32 :| []))
--                      fields
--                      Nothing
--              pure $
--                ConstantDefinition
--                  loc
--                  with
--                  (With trs t)
--                  ( EApplication
--                      mempty
--                      t
--                      (dictionaryLambda tr trs expr)
--                      recs
--                  )
--            else
--              pure $
--                ConstantDefinition loc with (With (tr : trs) t) (dictionaryLambda tr trs expr)

isVariable :: Trait IndexedType -> Bool
isVariable (Trait _ TVariable{}) = True
isVariable _ = False

-- dictionaryLambda ::
--  (Monoid a, HasType o k (Trait (Type o k))) =>
--  Trait (Type o k) ->
--  [Trait (Type o k)] ->
--  Expression a () (Type o k) ->
--  Expression a () (Type o k)
-- dictionaryLambda tr trs = ELambda mempty (dict <$> (tr :| trs))
-- where
--  dict t = PTraitInstance mempty (typeOf t) t

dictionaryLambda ::
  (Monoid a, HasType o k (Trait (Type o k))) =>
  Trait (Type o k) ->
  [Trait (Type o k)] ->
  Expression a i (Type o k) ->
  Expression a i (Type o k)
dictionaryLambda tr trs = ELambda mempty (dict <$> (tr :| trs))
 where
  dict t = PTraitInstance mempty (typeOf t) t

--

-- passiveOexpandConstantDefinitionTraits :: (Monad m, Monoid a, Data a, Show a) => Name -> ConstantDefinition a IndexedType -> CompilerT a m (ConstantDefinition a IndexedType)
-- passiveOexpandConstantDefinitionTraits name =
--  \case
--    ConstantDefinition loc with (With _ t) e -> do
--      (_, traits) <- listenDictionaryTraits (passiveOexpandTraitsInExpr e)
--      case Set.toList traits of
--        [] ->
--          pure $ ConstantDefinition loc with (With [] t) e
--        tr : trs -> do
--          path <- lift $ gets compilerCurrentPath
--          pure $ ConstantDefinition loc with (With (tr : trs) t) e

passiveOexpandLetDefinitionTraits :: (Monad m, Monoid a, Data a, Show a) => Name -> LetDefinition a Kind IndexedType -> CompilerT a m (LetDefinition a Kind IndexedType)
passiveOexpandLetDefinitionTraits name =
  \case
    LetDefinition loc with (With _ t) e -> do
      (_, traits) <- listenDictionaryTraits (passiveOexpandTraitsInExpr e)
      case Set.toList traits of
        [] ->
          pure $ LetDefinition loc with (With [] t) e
        tr : trs -> do
          path <- gets compilerCurrentPath
          pure $ LetDefinition loc with (With (tr : trs) t) e

passiveOexpandTraitsInExpr :: (Monad m, Monoid a, Data a, Data k, Show a) => Expression a k IndexedType -> CompilerT a m (Expression a k IndexedType)
passiveOexpandTraitsInExpr =
  \case
    ERecursiveLet a p e1 e2 ->
      expandRecursiveLet <$> passiveOexpandTraitsInExpr (ELet a (BPattern a p e1 :| []) e2)
    ELet a bs e -> do
      as <- censorDictionaryTraits (const mempty) (traverse transformBinding2 bs)
      let xs = concat (toList (snd <$> as))
      old <- get
      insertNamesC xs
      r <- ELet a (fst <$> as) <$> passiveOexpandTraitsInExpr e
      put old
      return r
    var@(EVariable _ (Label t name))
      | "$fold" `isPrefixOf` name -> do
          traits <- collectTraits t name
          tellDictionaryTraits traits
          pure var
    EVariable loc (Label t name) -> do
      traits <- collectTraits t name
      passiveOapplyTraits loc (Label t name) traits
      pure (EVariable loc (Label t name))
    ECompiledMatch a t e cs ->
      ECompiledMatch a t <$> passiveOexpandTraitsInExpr e <*> traverse passiveOexpandTraitsInClause cs
    e ->
      descendM passiveOexpandTraitsInExpr e

passiveOexpandTraitsInClause :: (Monad m, Monoid a, Data a, Data k, Show a) => CompiledClause a k IndexedType -> CompilerT a m (CompiledClause a k IndexedType)
passiveOexpandTraitsInClause =
  \case
    ECompiledClause a lls e ->
      ECompiledClause a lls <$> passiveOexpandTraitsInExpr e

passiveOapplyTraits :: forall m a. (Show a, Monoid a, Data a, Monad m) => a -> Label IndexedType -> Set (Trait IndexedType) -> CompilerT a m ()
passiveOapplyTraits loc (Label t name) traits = do
  case Set.toList traits of
    [] ->
      pure ()
    x : xs ->
      traverse_ insert_ (x :| xs)
 where
  insert_ trait = do
    fields <- lookupTraitInstance2 loc trait
    let zz = fields :: Maybe (Dictionary (Expression a Kind IndexedType))
    case fields of
      Nothing | isVariable trait -> do
        tellDictionaryTraits (Set.singleton trait)
      _ ->
        pure ()

cafe2 :: (Show a, Monad m, Monoid a, Data a) => Definition a Kind IndexedType -> CompilerT a m () -- [(Name, Set Name)]
cafe2 =
  \case
    DLet a name def -> do
      d <- passiveOexpandLetDefinitionTraits name def
      insertName2 (DLet a name d) name
    DInstance a InstanceDefinition{..} ->
      forM_ instanceDefinitionImplementations $
        \case
          DLet a name def -> do
            d <- passiveOexpandLetDefinitionTraits instanceName def
            insertName2 (DLet a name d) instanceName
           where
            instanceName = instanceLabel (Trait instanceDefinitionTraitName instanceDefinitionType) name
          _ ->
            pure ()
    _ ->
      pure ()

-- cafe :: (Show a, Monad m, Monoid a, Data a) => Definition a Kind IndexedType -> CompilerT a m () -- [(Name, Set Name)]
-- cafe =
--  \case
--    DConstant a name def x -> do
--      d <- passiveOexpandConstantDefinitionTraits name def
--      insertName (DConstant a name d x) name
--    DInstance a trait InstanceDefinition{..} ->
--      forM_ instanceDefinitionEntries $
--        \case
--          DConstant a name def x -> do
--            d <- passiveOexpandConstantDefinitionTraits instanceName def
--            insertName (DConstant a name d x) instanceName
--           where
--            instanceName = instanceLabel (Trait trait instanceDefinitionType) name
--          _ ->
--            pure ()
--    _ ->
--      pure ()
