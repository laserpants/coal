{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.TraitTransform where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable (foldrM)
import Data.List (nub)
import Lang.Common.Environment (Environment)
import Lang.Common.List1 (NonEmpty ((:|)))
import Lang.Label (Label (..))
import Lang.Utils (Name)
import Noll.Language
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..))
import Noll.SystemF.Substitution

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Lang.Common.Environment as Environment

borkZ ::
  ( MonadReader (Environment (Scheme TypeIndex Kind (Type TypeIndex Kind))) m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  ) =>
  Constant Expression a (Type TypeIndex Kind) ->
  m (Constant Expression a (Type TypeIndex Kind))
borkZ (Constant a u@(With _ t) e) = do
  (expr, traits) <- runWriterT (transformZ e)
  case nub traits of
    [] ->
      pure (Constant a u expr)
    tr : trs -> do
      pure (Constant a (With (tr : trs) t) (EDictionaryLambda undefined (tr :| trs) expr))

parameterized :: Trait (Type v k) -> Bool
parameterized (Trait _ TVariable{}) = True
parameterized _ = False

transformZ ::
  ( MonadReader (Environment (Scheme TypeIndex Kind (Type TypeIndex Kind))) m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  ) =>
  Expression a (Type TypeIndex Kind) ->
  m (Expression a (Type TypeIndex Kind))
transformZ =
  \case
    ELet a bs e -> do
      (as, traits) <- runWriterT (traverse transformBindingZ bs)
      let (ds, es) = NonEmpty.unzip as
      let xs = concat (NonEmpty.toList (snd <$> as)) -- :: [Scheme TypeIndex (Kind Int) (Type TypeIndex (Kind Int))]
      -- (as, traits) <- NonEmpty.unzip <$> traverse transformBindingZ bs
      -- (as, traits) <- NonEmpty.unzip <$> traverse transformBindingZ bs
      -- tell (filter (not . parameterized) traits)
      --      tell (filter parameterized traits)
      ELet a (fst <$> as) <$> local (Environment.insertMultiple xs) (transformZ e)
    expr@(EApplication a t var@(EVariable _ (Label t1 name)) es) -> do
      traits <- collectTraits t1 name
      case traits of
        [] ->
          EApplication a t var <$> traverse transformZ es
        tr : trs -> do
          -- index <- fresh
          tell (filter parameterized traits)
          pure undefined -- (EDictionaryApplication a t var (tr :| trs) (NonEmpty.toList es) expr)
    expr@(EVariable a ll@(Label t name)) -> do
      traits <- collectTraits t name
      case traits of
        [] ->
          pure (EVariable a ll)
        tr : trs -> do
          -- index <- fresh
          tell (filter parameterized traits)
          pure undefined -- (EDictionaryApplication t expr (tr :| trs) [] expr)
    EListLiteral a t es ->
      EListLiteral a t <$> traverse transformZ es
    ELambda a ps e ->
      ELambda a ps <$> transformZ e
    EApplication a t e1 es ->
      EApplication a t <$> transformZ e1 <*> traverse transformZ es

--    EDictionaryApplication t e1 ts es e2 -> do
--      -- tell (NonEmpty.toList ts)
--      EDictionaryApplication t
--        <$> transformZ e1
--        <*> pure ts
--        <*> traverse transformZ es
--        <*> pure e2
--    ECompiledMatch t e cs ->
--      ECompiledMatch t
--        <$> transformZ e
--        <*> traverse transformCompiledClauseZ cs
--    EFold t e1 cs me ->
--      EFold t
--        <$> traverse transformZ e1
--        <*> traverse transformClauseZ cs
--        <*> traverse transformZ me
--    ERecord t d me ->
--      ERecord t
--        <$> traverse transformZ d
--        <*> traverse transformZ me
--    EIf e1 e2 e3 ->
--      EIf
--        <$> transformZ e1
--        <*> transformZ e2
--        <*> transformZ e3
--    EConstructor ll ->
--      pure (EConstructor ll)
--    expr@ELiteral{} ->
--      pure expr
--    expr@EBinaryOperator{} ->
--      pure expr
--    ESelect t name e ->
--      ESelect t name <$> transformZ e
--    EListCons t e1 e2 ->
--      EListCons t
--        <$> transformZ e1
--        <*> transformZ e2
----    EBlock es ->
----      EBlock <$> traverse transformZ es
--    expr ->
--      error (show expr) -- pure expr
--      -- pure expr
--

transformBindingZ ::
  ( MonadReader (Environment (Scheme TypeIndex Kind (Type TypeIndex Kind))) m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  ) =>
  Binding Expression a (Type TypeIndex Kind) ->
  m (Binding Expression a (Type TypeIndex Kind), [(Name, Scheme TypeIndex Kind (Type TypeIndex Kind))])
transformBindingZ =
  \case
    BPattern a var@(PVariable a1 (Label t name)) e
      | Text.isPrefixOf "$fold" name -> do
          body <- transformZ e
          pure (BPattern a var body, [])
    BPattern a var@(PVariable a1 (Label t name)) e -> do
      (body, traits) <- runWriterT (transformZ e)
      case nub traits of
        [] ->
          pure (BPattern a var body, [])
        tr : trs -> do
          -- tell traits
          pure (BPattern a var (EDictionaryLambda a (tr :| trs) body), [(name, Forall (typeIndexesIn t) traits t)])
    -- (body, traits) <- runWriterT (transform e)
    -- case traits of
    --  [] ->
    --    pure (BPattern var body, [])
    --  tr : trs ->
    --    pure (BPattern var (EDictionaryLambda (tr :| trs) body), [(name, tr : trs)])
    BPattern a p e ->
      error "TODO"

collectTraits ::
  ( MonadReader (Environment (Scheme TypeIndex Kind (Type TypeIndex Kind))) m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  ) =>
  Type TypeIndex Kind ->
  Name ->
  m [Trait (Type TypeIndex Kind)]
collectTraits u name = do
  env <- ask
  case Environment.lookup name env of
    Nothing ->
      pure []
    Just (Forall vs ts t) -> do
      sub1 <- foldrM instantiate mempty vs
      r <- tryMatch (apply sub1 t) u
      case r of
        Left x ->
          error "TODO" -- (show x) -- "???"
        Right sub2 ->
          pure (apply sub2 (apply sub1 ts))
 where
  instantiate (TypeIndex k index) acc = do
    undefined

--    var <- (TVariable <$$> TypeIndex) k <$> fresh
--    pure (index `mapsTo` var <> acc)

tryMatch ::
  (MonadState Int m) =>
  Type TypeIndex Kind ->
  Type TypeIndex Kind ->
  m (Either e Substitution)
tryMatch t u = undefined -- runMatch <$> fresh <*> pure (match t u)
