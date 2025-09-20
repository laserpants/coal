{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Transform.Dictionaries (TraitContext (..), transformScope, collectTraits) where

import qualified Coal.Common.Environment as Environment
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (supplied)
import Coal.Compiler.Environment (overCompilerDictionaryNameEnvironment)
import Coal.Compiler.Journal (censorDictionaryTraits, listenDictionaryTraits, tellDictionaryTraits)
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Coal.TypeSystem.Substitution
import Coal.TypeSystem.Unification
import Control.Monad (forM)
import Control.Monad.Reader (asks, local)
import Data.Data (Data)
import Data.Foldable (foldrM)
import Data.Generics.Uniplate.Data (descendM)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Text (isPrefixOf)
import Extra (Dictionary, Name)

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

findFirstMatch :: (Monad m) => Trait IndexedType -> CompilerT a m (Maybe (ParameterizedType, IndexedType, Map Name IndexedScheme))
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
    \(k, (t1, env)) -> do
      result <- f k
      case result of
        Left{} ->
          pure Nothing
        Right sub ->
          pure $ Just (t1, k, Map.map (substituteInScheme sub) env)

substituteInScheme :: Substitution -> Scheme o Kind IndexedType -> IndexedScheme
substituteInScheme sub (Forall _ ts t) = scheme (apply sub ts) (apply sub t)

mapEntriesM :: (Monad m) => Dictionary IndexedScheme -> ((Name, IndexedScheme) -> m (Name, Expression a IndexedType)) -> m (Maybe (Dictionary (Expression a IndexedType)))
mapEntriesM d f = Just . Map.fromList <$> traverse f (Map.toList d)

lookupTraitInstance :: (Monoid a, Monad m) => Trait IndexedType -> CompilerT a m (Maybe (Map Name (Expression a IndexedType)))
lookupTraitInstance trait@(Trait name _) = do
  found <- findFirstMatch trait
  case found of
    Nothing ->
      pure Nothing
    Just (t, a, b) ->
      mapEntriesM b (uncurry (go t (Trait name a)))
 where
  go t1 (Trait tn _) n (Forall _ ts t) = do
    expr <- applyTraits (Label t (instanceLabel (Trait tn t1) n)) ts
    pure (n, expr)

applyTraits :: (Monoid a, Monad m) => Label IndexedType -> [Trait IndexedType] -> CompilerT a m (Expression a IndexedType)
applyTraits (Label t name) =
  \case
    [] ->
      pure (EVariable mempty (Label t name))
    tr : trs ->
      EApplication mempty t (EVariable mempty (Label t1 name)) <$> traverse insertInstance (tr :| trs)
     where
      t1 = foldTypeOf t (tr : trs)
      insertInstance trait = do
        fields <- lookupTraitInstance trait
        case fields of
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
      EVariable _ (Label t name) -> do
        traits <- collectTraits t name
        applyTraits (Label t name) (nub traits)
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
      let ll = Label (foldTypeOf t traits) name
      pure (BPattern mempty (PVariable a ll) e1, [(name, Forall (typeIndexesIn t) traits t)])
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
      ECompiledClause lls e ->
        ECompiledClause lls <$> expandTraits e

instance (Monoid a, Data a) => TraitContext a (Module a Kind IndexedType) where
  expandTraits =
    overModuleDefinitionsM (traverse expandTraits)

instance (Monoid a, Data a) => TraitContext a (Definition a Kind IndexedType) where
  expandTraits =
    \case
      DConstant loc name c fs ->
        DConstant loc name <$> expandTraits c <*> traverse expandTraits fs
      DFold loc name (FoldDef with cs (Just e)) ->
        DFold loc name . FoldDef with cs . Just <$> expandTraits e
      DUnfold loc name (UnfoldDef with ps d (Just e)) ->
        DUnfold loc name . UnfoldDef with ps d . Just <$> expandTraits e
      d ->
        pure d

instance (Monoid a, Data a) => TraitContext a (ConstantDef a IndexedType) where
  expandTraits =
    \case
      ConstantDef a with (With _ t) e -> do
        (expr, traits) <- listenDictionaryTraits (expandTraits e)
        case nub traits of
          [] ->
            pure $ ConstantDef a with (With [] t) expr
          tr : trs ->
            pure $ ConstantDef a with (With (tr : trs) t) (dictionaryLambda tr trs expr)

dictionaryLambda :: (Monoid a, HasType o k (Trait (Type o k))) => Trait (Type o k) -> [Trait (Type o k)] -> Expression a (Type o k) -> Expression a (Type o k)
dictionaryLambda tr trs = ELambda mempty (dict <$> (tr :| trs))
 where
  dict t = PTraitDictionary mempty (typeOf t) t
