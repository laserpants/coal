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
import Data.List (nub, sort)
import Lang.Common.Environment (Environment)
import Lang.Common.List1 (NonEmpty ((:|)))
import Lang.Common.Supply (Supply (..), supplied)
import Lang.Label (Label (..))
import Lang.Utils (Name, (<$$>))
import Noll.Language
import Noll.Module
import Noll.SystemF.Substitution
import Noll.SystemF.Unification

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Lang.Common.Environment as Environment
import qualified Lang.Common.List1 as List1

transformModuleZ ::
  ( MonadReader (Environment (Scheme TypeIndex Kind (Type TypeIndex Kind))) m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  , Show a
  ) =>
  Module a Kind (Type TypeIndex Kind) ->
  m (Module a Kind (Type TypeIndex Kind))
transformModuleZ = overModuleDefinitionsM (traverse transformDefinitionZ)

transformDefinitionZ ::
  ( MonadReader (Environment (Scheme TypeIndex Kind (Type TypeIndex Kind))) m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  , Show a
  ) =>
  Definition a Kind (Type TypeIndex Kind) ->
  m (Definition a Kind (Type TypeIndex Kind))
transformDefinitionZ =
  \case
    DConstant name c ->
      DConstant name <$> transformConstantZ c
    DAnnotation a d ->
      DAnnotation a <$> transformDefinitionZ d
    d ->
      pure d

transformConstantZ ::
  ( MonadReader (Environment (Scheme TypeIndex Kind (Type TypeIndex Kind))) m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  , Show a
  ) =>
  Constant Expression a (Type TypeIndex Kind) ->
  m (Constant Expression a (Type TypeIndex Kind))
transformConstantZ (Constant a u@(With _ t) e) = do
  (expr, traits) <- runWriterT (transformZ e)
  case nub traits of
    [] ->
      pure (Constant a u expr)
    tr : trs -> do
      pure (Constant a (With (sort (tr : trs)) t) (EDictionaryLambda a (List1.sort (tr :| trs)) expr))

parameterized :: Trait (Type v k) -> Bool
parameterized =
  \case
    Trait _ TVariable{} ->
      True
    _ ->
      False

-- TODO: Use uniplate?

transformZ ::
  ( MonadReader (Environment (Scheme TypeIndex Kind (Type TypeIndex Kind))) m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  , Show a
  ) =>
  Expression a (Type TypeIndex Kind) ->
  m (Expression a (Type TypeIndex Kind))
transformZ =
  \case
    ERecursiveLet a p e1 e2 ->
      transformZ (ELet a (BPattern a p e1 :| []) e2)
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
          ds <- traverse transformZ es
          pure (EDictionaryApplication a t var (List1.nub (tr :| trs)) (NonEmpty.toList ds))
    expr@(EVariable a ll@(Label t name)) -> do
      traits <- collectTraits t name
      case traits of
        [] ->
          pure (EVariable a ll)
        tr : trs -> do
          -- index <- fresh
          tell (filter parameterized traits)
          pure (EDictionaryApplication a t expr (List1.nub (tr :| trs)) [])
    EListLiteral a t es ->
      EListLiteral a t <$> traverse transformZ es
    ELambda a ps e ->
      ELambda a ps <$> transformZ e
    EApplication a t e1 es ->
      EApplication a t <$> transformZ e1 <*> traverse transformZ es
    EDictionaryApplication a t e1 ts es -> do
      -- tell (NonEmpty.toList ts)
      EDictionaryApplication a t
        <$> transformZ e1
        <*> pure ts
        <*> traverse transformZ es
    ECompiledMatch a t e cs ->
      ECompiledMatch a t
        <$> transformZ e
        <*> traverse transformCompiledClauseZ cs
    EFold a t e1 cs me ->
      EFold a t
        <$> traverse transformZ e1
        <*> traverse transformClauseZ cs
        <*> traverse transformZ me
    ERecord a t d me ->
      ERecord a t
        <$> traverse transformZ d
        <*> traverse transformZ me
    EIf a t e1 e2 e3 ->
      EIf a t
        <$> transformZ e1
        <*> transformZ e2
        <*> transformZ e3
    EConstructor a ll ->
      pure (EConstructor a ll)
    expr@ELiteral{} ->
      pure expr
    expr@EBinaryOperator{} ->
      pure expr
    ESelect a ll e ->
      ESelect a ll <$> transformZ e
    EFocus name ll1 ll2 e1 e2 ->
      EFocus name ll1 ll2 <$> transformZ e1 <*> transformZ e2
    EListCons a t e1 e2 ->
      EListCons a t
        <$> transformZ e1
        <*> transformZ e2
    --    EBlock es ->
    --      EBlock <$> traverse transformZ es
    EAnnotation a t e ->
      EAnnotation a t <$> transformZ e
    expr ->
      error (show expr) -- "TODO" -- pure expr
      -- pure expr

transformGuardZ ::
  ( MonadReader (Environment (Scheme TypeIndex Kind (Type TypeIndex Kind))) m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  , Show a
  ) =>
  Guard Expression a (Type TypeIndex Kind) ->
  m (Guard Expression a (Type TypeIndex Kind))
transformGuardZ =
  \case
    CGuard e ->
      CGuard <$> transformZ e

transformChoiceZ ::
  ( MonadReader (Environment (Scheme TypeIndex Kind (Type TypeIndex Kind))) m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  , Show a
  ) =>
  Choice Expression a (Type TypeIndex Kind) ->
  m (Choice Expression a (Type TypeIndex Kind))
transformChoiceZ =
  \case
    CPlain a gs e ->
      CPlain a <$> traverse transformGuardZ gs <*> transformZ e

transformClauseZ ::
  ( MonadReader (Environment (Scheme TypeIndex Kind (Type TypeIndex Kind))) m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  , Show a
  ) =>
  Clause a (Type TypeIndex Kind) ->
  m (Clause a (Type TypeIndex Kind))
transformClauseZ =
  \case
    EClause a ps cs ->
      EClause a ps <$> traverse transformChoiceZ cs

transformCompiledClauseZ ::
  ( MonadReader (Environment (Scheme TypeIndex Kind (Type TypeIndex Kind))) m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  , Show a
  ) =>
  CompiledClause a (Type TypeIndex Kind) ->
  m (CompiledClause a (Type TypeIndex Kind))
transformCompiledClauseZ =
  \case
    ECompiledClause lls e ->
      ECompiledClause lls <$> transformZ e

--    RField name ll1 ll2 e ->
--      RField name ll1 ll2 <$> transformZ e

transformBindingZ ::
  ( MonadReader (Environment (Scheme TypeIndex Kind (Type TypeIndex Kind))) m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  , Show a
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
          pure (BPattern a var (EDictionaryLambda a (List1.sort (tr :| trs)) body), [(name, Forall (typeIndexesIn t) traits t)])
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
          error (show (name, apply sub1 t, u)) -- "TODO" -- (show x) -- "???"
        Right sub2 ->
          pure (apply sub2 (apply sub1 ts))
 where
  instantiate (TypeIndex k index) acc = do
    var <- supplied (TVariable . TypeIndex KType)
    --    var <- (TVariable <$$> TypeIndex) k <$> fresh
    pure (index `mapsTo` var <> acc)

tryMatch ::
  (MonadState Int m) =>
  Type TypeIndex Kind ->
  Type TypeIndex Kind ->
  m (Either UnificationError Substitution)
tryMatch t u = do
  var <- supplied id
  -- let zz = match t u
  pure (evalUnifier var (match t u))
