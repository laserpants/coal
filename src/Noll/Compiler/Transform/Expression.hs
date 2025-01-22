{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Noll.Compiler.Transform.Expression (
  mapOverExpression,
  mapMOverExpression,
) where

import Control.Monad ((<=<))
import Control.Monad.Identity (runIdentity)
import Data.Map.Strict (Map)
import Noll.Common.List1 (NonEmpty)
import Noll.Language (
  Binding (..),
  Choice (..),
  Clause (..),
  CompiledClause (..),
  Expression (..),
  Guard (..),
 )

mapOverExpression :: (Expression a t -> Expression a t) -> Expression a t -> Expression a t
mapOverExpression f = runIdentity . overExpression (pure . f)

mapMOverExpression :: (Monad m) => (Expression a t -> m (Expression a t)) -> Expression a t -> m (Expression a t)
mapMOverExpression = overExpression

class ExpressionContext o e where
  overExpression :: (Monad m) => (o -> m o) -> e -> m e

instance ExpressionContext (Expression a t) (Expression a t) where
  overExpression f =
    \case
      EAnnotation a t e1 -> do
        EAnnotation a t <$> (overExpression f =<< f e1)
      EApplication a t e1 es ->
        EApplication a t
          <$> (overExpression f =<< f e1)
          <*> (overExpression f =<< traverse f es)
      EIf a t e1 e2 e3 ->
        EIf a t
          <$> (overExpression f =<< f e1)
          <*> (overExpression f =<< f e2)
          <*> (overExpression f =<< f e3)
      EListLiteral a t es ->
        EListLiteral a t
          <$> (overExpression f =<< traverse f es)
      ELambda a ps e -> do
        ELambda a ps
          <$> (overExpression f =<< f e)
      ELet a gs e ->
        ELet a
          <$> overExpression f gs
          <*> (overExpression f =<< f e)
      ERecursiveLet a p e1 e2 ->
        ERecursiveLet a p
          <$> (overExpression f =<< f e1)
          <*> (overExpression f =<< f e2)
      EMatch a t e cs ->
        EMatch a t
          <$> (overExpression f =<< f e)
          <*> overExpression f cs
      ECompiledMatch a t e cs ->
        ECompiledMatch a t
          <$> (overExpression f =<< f e)
          <*> overExpression f cs
      EListCons a t e1 e2 ->
        EListCons a t
          <$> (overExpression f =<< f e1)
          <*> (overExpression f =<< f e2)
      EListLiteral a t es ->
        EListLiteral a t
          <$> (overExpression f =<< traverse f es)
      ERecord a t d e ->
        ERecord a t
          <$> (overExpression f =<< traverse f d)
          <*> (overExpression f =<< traverse f e)
      e@EUnaryOperator{} ->
        pure e
      e@EBinaryOperator{} ->
        pure e
      e@EVariable{} ->
        pure e
      e@EConstructor{} ->
        pure e
      e@ELiteral{} ->
        pure e

instance (ExpressionContext d e) => ExpressionContext d [e] where
  overExpression = traverse . overExpression

instance (ExpressionContext d e) => ExpressionContext d (NonEmpty e) where
  overExpression = traverse . overExpression

instance (ExpressionContext d d) => ExpressionContext d (Map e d) where
  overExpression = traverse . overExpression

instance (ExpressionContext d e) => ExpressionContext d (Maybe e) where
  overExpression = traverse . overExpression

instance ExpressionContext (Expression a t) (Binding Expression a t) where
  overExpression f =
    \case
      BPattern a p e ->
        BPattern a p <$> (overExpression f =<< f e)
      BFunction{} ->
        error "TODO"

instance ExpressionContext (Expression a t) (Clause Expression a t) where
  overExpression f =
    \case
      EClause a p cs ->
        EClause a p <$> overExpression f cs

instance ExpressionContext (Expression a t) (CompiledClause Expression a t) where
  overExpression f =
    \case
      ECompiledClause lls e -> do
        ECompiledClause lls <$> (overExpression f =<< f e)

instance ExpressionContext (Expression a t) (Choice Expression a t) where
  overExpression f =
    \case
      CPlain a gs e ->
        CPlain a
          <$> overExpression f gs
          <*> (overExpression f =<< f e)
      CLambda a ps gs e ->
        CLambda a ps
          <$> overExpression f gs
          <*> (overExpression f =<< f e)

instance ExpressionContext (Expression a t) (Guard Expression a t) where
  overExpression f =
    \case
      CGuard e ->
        CGuard <$> (overExpression f =<< f e)
