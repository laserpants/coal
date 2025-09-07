{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Transform.Dictionaries (
  TraitContext (..),
  DictionaryEnvironment (..),
  DictionaryStack (..),
  runDictionaryStack,
  transformScope,
  collectTraits,
) where

import Coal.Common.Environment (Environment (..))
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (supplied)
import Coal.Language
import Coal.Language.Module
import Coal.TypeSystem.Substitution
import Coal.TypeSystem.Unification
import Control.Monad (forM)
import Control.Monad.RWS (RWS, runRWS)
import Control.Monad.Reader (MonadReader, asks, local)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadWriter, censor, listen, tell)
import Data.Data (Data)
import Data.Foldable (foldrM)
import Data.Generics.Uniplate.Data (descendM)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Text (isPrefixOf)
import Extra (Dictionary, Name)

import qualified Coal.Common.Environment as Environment
import qualified Data.Map.Strict as Map

data DictionaryEnvironment = DictionaryEnvironment
  { dictionaryEnvironmentNames :: Environment IndexedScheme
  , dictionaryEnvironmentInstances :: Environment (Map IndexedType (Type Parameter (), Dictionary IndexedScheme))
  }
  deriving (Show, Eq, Ord, Read)

overDictionaryEnvironmentNames ::
  ( Environment IndexedScheme ->
    Environment IndexedScheme
  ) ->
  DictionaryEnvironment ->
  DictionaryEnvironment
overDictionaryEnvironmentNames f DictionaryEnvironment{..} =
  DictionaryEnvironment{dictionaryEnvironmentNames = f dictionaryEnvironmentNames, ..}

newtype DictionaryStack a = DictionaryStack {dictionaryStack :: RWS DictionaryEnvironment [Trait IndexedType] Int a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader DictionaryEnvironment
    , MonadState Int
    , MonadWriter [Trait IndexedType]
    )

runDictionaryStack :: DictionaryEnvironment -> Int -> DictionaryStack a -> (a, Int)
runDictionaryStack e s d = (a, n)
 where
  (a, n, _) = runRWS (dictionaryStack d) e s

collectTraits :: IndexedType -> Name -> DictionaryStack [Trait IndexedType]
collectTraits u name = do
  env <- asks dictionaryEnvironmentNames
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

tryMatch :: IndexedType -> IndexedType -> DictionaryStack (Either UnificationError Substitution)
tryMatch t u = do
  var <- supplied id
  pure (evalUnifier var (match t u))

findFirstMatch :: Trait IndexedType -> DictionaryStack (Maybe (Type Parameter (), IndexedType, Map Name IndexedScheme))
findFirstMatch (Trait name t1) = do
  env <- asks dictionaryEnvironmentInstances
  case Environment.lookup name env of
    Nothing ->
      pure Nothing
    Just env1 -> do
      kvs <- go (`tryMatch` t1) env1
      case kvs of
        [] ->
          pure Nothing
        (x, k, v) : _ ->
          pure (Just (x, k, v))
 where
  go f m = fmap catMaybes . forM (Map.toList m) $
    \(k, (x, env)) -> do
      result <- f k
      pure $
        case result of
          Left{} ->
            Nothing
          Right sub ->
            Just (x, k, Map.map (substituteInScheme sub) env)

mapEntriesM :: (Monad m) => Dictionary IndexedScheme -> ((Name, IndexedScheme) -> m (Name, Expression a IndexedType)) -> m (Maybe (Dictionary (Expression a IndexedType)))
mapEntriesM b f = Just . Map.fromList <$> traverse f (Map.toList b)

lookupTraitInstance :: (Monoid a) => Trait IndexedType -> DictionaryStack (Maybe (Map Name (Expression a IndexedType)))
lookupTraitInstance tr@(Trait name _) = do
  found <- findFirstMatch tr
  case found of
    Nothing ->
      pure Nothing
    Just (x, a, b) ->
      mapEntriesM b (uncurry (go x (Trait name a)))
 where
  go x trait@(Trait tn _) n (Forall _ ts t) = do
    expr <- applyTraits (Label t (n <> "__$instance_" <> serialize (Trait tn x))) ts
    pure (n, expr)

applyTraits :: (Monoid a) => Label IndexedType -> [Trait IndexedType] -> DictionaryStack (Expression a IndexedType)
applyTraits ll@(Label t name) =
  \case
    [] ->
      pure (EVariable mempty ll)
    tr : trs ->
      EApplication mempty t (EVariable mempty (Label t1 name)) <$> traverse insertInstance (tr :| trs)
     where
      t1 = foldType t (typeOf <$> (tr : trs))
      insertInstance trait = do
        mm <- lookupTraitInstance trait
        case mm of
          Nothing -> do
            tell [trait]
            pure (ETraitDictionary mempty (typeOf trait) trait)
          Just r ->
            pure (ERecord mempty (typeOf trait) r Nothing)

class TraitContext d where
  expandTraits :: d -> DictionaryStack d

instance (Monoid a, Data a) => TraitContext (Expression a IndexedType) where
  expandTraits =
    \case
      ERecursiveLet a p e1 e2 -> do
        res <- expandTraits (ELet a (BPattern a p e1 :| []) e2)
        case res of
          ELet a2 (BPattern _ p2 e8 :| []) e9 ->
            pure (ERecursiveLet a2 p2 e8 e9)
          _ ->
            error "Implementation error"
      ELet a bs e -> do
        as <- censor (const []) (traverse transformBinding bs)
        let xs = concat (toList (snd <$> as))
        ELet a (fst <$> as) <$> local (overDictionaryEnvironmentNames (Environment.insertMultiple xs)) (expandTraits e)
      EVariable _ ll@(Label t name) -> do
        traits <- collectTraits t name
        applyTraits ll (nub traits)
      ECompiledMatch a t e cs ->
        ECompiledMatch a t
          <$> expandTraits e
          <*> traverse expandTraits cs
      EFold a t es cs (Just e) -> do
        e1 <- descendM expandTraits e
        pure (EFold a t es cs (Just e1))
      e ->
        descendM expandTraits e
   where
    transformBinding =
      \case
        BPattern a var@(PVariable _ (Label _ name)) e
          | "$fold" `isPrefixOf` name -> do
              body <- expandTraits e
              pure (BPattern a var body, [])
        BPattern _ (PVariable a (Label t name)) e -> do
          (e1, traits) <- transformScope e
          let t1 = foldType t (typeOf <$> traits)
          pure (BPattern mempty (PVariable a (Label t1 name)) e1, [(name, Forall (typeIndexesIn t) traits t)])
        _ ->
          error "Not implemented"

transformScope :: (Monoid a, Data a) => Expression a IndexedType -> DictionaryStack (Expression a IndexedType, [Trait IndexedType])
transformScope e = do
  (expr, traits) <- listen (expandTraits e)
  case nub traits of
    [] ->
      pure (expr, traits)
    tr : trs ->
      pure (ELambda mempty (toPattern <$> (tr :| trs)) expr, traits)
 where
  toPattern tr =
    PTraitDictionary mempty (typeOf tr) tr

instance (Monoid a, Data a) => TraitContext (CompiledClause a IndexedType) where
  expandTraits =
    \case
      ECompiledClause lls e ->
        ECompiledClause lls <$> expandTraits e

instance (Monoid a, Data a) => TraitContext (Module a Kind IndexedType) where
  expandTraits =
    overModuleDefinitionsM (traverse expandTraits)

instance (Monoid a, Data a) => TraitContext (Definition a Kind IndexedType) where
  expandTraits =
    \case
      DConstant loc name c fs ->
        DConstant loc name <$> expandTraits c <*> traverse expandTraits fs
      DUnfold loc name (UnfoldDef with ps d (Just e)) -> do
        e1 <- expandTraits e
        pure $ DUnfold loc name (UnfoldDef with ps d (Just e1))
      d ->
        pure d

instance (Monoid a, Data a) => TraitContext (ConstantDef a IndexedType) where
  expandTraits =
    \case
      ConstantDef a w1 (With _ t) e -> do
        (expr, traits) <- listen (expandTraits e)
        pure $
          case nub traits of
            [] ->
              ConstantDef a w1 (With [] t) expr
            tr : trs ->
              ConstantDef
                a
                w1
                (With (tr : trs) t)
                (ELambda mempty (toPattern <$> (tr :| trs)) expr)
       where
        toPattern tr = PTraitDictionary mempty (typeOf tr) tr
