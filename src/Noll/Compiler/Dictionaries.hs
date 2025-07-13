{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Dictionaries (
  TraitContext (..),
  DictionaryEnvironment (..),
  DictionaryStack (..),
  runDictionaryStack,
  transformScope,
  collectTraits,
) where

import Control.Monad (forM)
import Control.Monad.RWS (RWS, runRWS)
import Control.Monad.Reader (MonadReader, asks, local)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadWriter, listen, tell)
import Data.Data (Data)
import Data.Foldable (foldrM)
import Data.Generics.Uniplate.Data (descendM)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Lang.Common.Environment (Environment (..))
import Lang.Common.Supply (supplied)
import Lang.Label (Label (..))
import Lang.Utils (Dictionary, Name)
import Noll.Language
import Noll.Module
import Noll.SystemF.Substitution
import Noll.SystemF.Unification
import Noll.Utils (serialize)

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Lang.Common.Environment as Environment
import Lang.Common.List1 (NonEmpty (..), fromList1)

data DictionaryEnvironment = DictionaryEnvironment
  { dictionaryEnvironmentNames :: Environment (Scheme TypeIndex Kind (Type TypeIndex Kind))
  , dictionaryEnvironmentInstances :: Environment (Map IndexedType (Dictionary (Scheme TypeIndex Kind IndexedType)))
  }
  deriving (Show, Eq, Ord, Read)

overDictionaryEnvironmentNames ::
  ( Environment (Scheme TypeIndex Kind (Type TypeIndex Kind)) ->
    Environment (Scheme TypeIndex Kind (Type TypeIndex Kind))
  ) ->
  DictionaryEnvironment ->
  DictionaryEnvironment
overDictionaryEnvironmentNames f DictionaryEnvironment{..} =
  DictionaryEnvironment{dictionaryEnvironmentNames = f dictionaryEnvironmentNames, ..}

newtype DictionaryStack a = DictionaryStack {dictionaryStack :: RWS DictionaryEnvironment [Trait (Type TypeIndex Kind)] Int a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader DictionaryEnvironment
    , MonadState Int
    , MonadWriter [Trait (Type TypeIndex Kind)]
    )

runDictionaryStack :: DictionaryEnvironment -> Int -> DictionaryStack a -> (a, Int)
runDictionaryStack e s d = (a, n)
 where
  (a, n, _) = runRWS (dictionaryStack d) e s

collectTraits :: Type TypeIndex Kind -> Name -> DictionaryStack [Trait (Type TypeIndex Kind)]
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
  instantiate (TypeIndex _ index) acc = do
    var <- supplied (TVariable . TypeIndex KType)
    pure (index `mapsTo` var <> acc)

tryMatch :: Type TypeIndex Kind -> Type TypeIndex Kind -> DictionaryStack (Either UnificationError Substitution)
tryMatch t u = do
  var <- supplied id
  pure (evalUnifier var (match t u))

findFirstMatch :: Trait (Type TypeIndex Kind) -> DictionaryStack (Maybe (Type TypeIndex Kind, Map Name (Scheme TypeIndex Kind IndexedType)))
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
        (k, v) : _ ->
          pure (Just (k, v))
 where
  go f m = fmap catMaybes . forM (Map.toList m) $
    \(k, env) -> do
      result <- f k
      case result of
        Left{} -> pure Nothing
        Right sub -> pure $ Just (k, Map.map (applySpecial sub) env)

applySpecial :: Substitution -> Scheme TypeIndex Kind IndexedType -> Scheme TypeIndex Kind IndexedType
applySpecial sub (Forall _ ts t) = Forall (typeIndexesIn t' <> typeIndexesIn ts') ts' t'
 where
  t' = apply sub t
  ts' = apply sub ts

mapEntriesM :: (Monad m) => Dictionary (Scheme TypeIndex Kind IndexedType) -> ((Name, Scheme TypeIndex Kind IndexedType) -> m (Name, Expression a (Type TypeIndex Kind))) -> m (Maybe (Dictionary (Expression a (Type TypeIndex Kind))))
mapEntriesM b f = Just . Map.fromList <$> traverse f (Map.toList b)

lookupTraitInstance :: (Monoid a) => Trait (Type TypeIndex Kind) -> DictionaryStack (Maybe (Map Name (Expression a (Type TypeIndex Kind))))
lookupTraitInstance tr@(Trait name _) = do
  found <- findFirstMatch tr
  case found of
    Nothing ->
      pure Nothing
    Just (a, b) ->
      mapEntriesM b (uncurry (go (Trait name a)))
 where
  go trait n (Forall _ ts t) = do
    expr <- applyTraits (Label t (n <> "__$instance." <> serialize trait)) ts
    pure (n, expr)

applyTraits :: (Monoid a) => Label (Type TypeIndex Kind) -> [Trait (Type TypeIndex Kind)] -> DictionaryStack (Expression a (Type TypeIndex Kind))
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
            pure (EPlaceholder mempty (typeOf trait) trait)
          Just r ->
            pure (ERecord mempty (typeOf trait) r Nothing)

class TraitContext d where
  expandTraits :: d -> DictionaryStack d

instance (Monoid a, Data a) => TraitContext (Expression a (Type TypeIndex Kind)) where
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
        -- (as, traits) <- listen (traverse transformBinding bs)
        -- let (ds, es) = List1.unzip as
        as <- traverse transformBinding bs
        let xs = concat (fromList1 (snd <$> as))
        ELet a (fst <$> as) <$> local (overDictionaryEnvironmentNames (Environment.insertMultiple xs)) (expandTraits e)
      EVariable _ ll@(Label t name) ->
        applyTraits ll =<< collectTraits t name
      ECompiledMatch a t e cs ->
        ECompiledMatch a t
          <$> expandTraits e
          <*> traverse expandTraits cs
      e ->
        descendM expandTraits e
   where
    transformBinding =
      \case
        BPattern a var@(PVariable _ (Label _ name)) e
          | Text.isPrefixOf "$fold" name -> do
              body <- expandTraits e
              pure (BPattern a var body, [])
        BPattern _ var@(PVariable _ (Label t name)) e -> do
          (e1, traits) <- transformScope e
          pure (BPattern mempty var e1, [(name, Forall (typeIndexesIn t) traits t)])
        _ ->
          error "Not implemented"

transformScope :: (Monoid a, Data a) => Expression a (Type TypeIndex Kind) -> DictionaryStack (Expression a (Type TypeIndex Kind), [Trait (Type TypeIndex Kind)])
transformScope e = do
  (expr, traits) <- listen (expandTraits e)
  case nub traits of
    [] ->
      pure (expr, traits)
    tr : trs ->
      pure (ELambda mempty (toPattern <$> (tr :| trs)) expr, traits)
 where
  toPattern tr =
    PPlaceholder mempty (typeOf tr) tr

instance (Monoid a, Data a) => TraitContext (CompiledClause a (Type TypeIndex Kind)) where
  expandTraits =
    \case
      ECompiledClause lls e ->
        ECompiledClause lls <$> expandTraits e

instance (Monoid a, Data a) => TraitContext (Module a Kind (Type TypeIndex Kind)) where
  expandTraits =
    overModuleDefinitionsM (traverse expandTraits)

instance (Monoid a, Data a) => TraitContext (Definition a Kind (Type TypeIndex Kind)) where
  expandTraits =
    \case
      DConstant name c ->
        DConstant name <$> expandTraits c
      DAnnotation a d ->
        DAnnotation a <$> expandTraits d
      d ->
        pure d

instance (Monoid a, Data a) => TraitContext (Constant Expression a (Type TypeIndex Kind)) where
  expandTraits =
    \case
      Constant a (With _ t) e -> do
        (expr, traits) <- listen (descendM expandTraits e)
        case nub traits of
          [] ->
            pure (Constant a (With [] t) expr)
          tr : trs ->
            pure
              ( Constant
                  a
                  (With (tr : trs) t)
                  (ELambda mempty (toPattern <$> (tr :| trs)) expr)
              )
       where
        toPattern tr = PPlaceholder mempty (typeOf tr) tr
