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
    f
      <=< \case
        EAnnotation a t e1 ->
          EAnnotation a t <$> overExpression f e1
        EApplication a t e1 es ->
          EApplication a t <$> overExpression f e1 <*> overExpression f es
        EIf a t e1 e2 e3 ->
          EIf a t
            <$> overExpression f e1
            <*> overExpression f e2
            <*> overExpression f e3
        EListLiteral a t es ->
          EListLiteral a t <$> overExpression f es
        ELambda a ps e ->
          ELambda a ps <$> overExpression f e
        ELet a gs e ->
          ELet a <$> overExpression f gs <*> overExpression f e
        EMatch a t e cs ->
          EMatch a t <$> overExpression f e <*> overExpression f cs

instance (ExpressionContext d e) => ExpressionContext d [e] where
  overExpression = traverse . overExpression

instance (ExpressionContext d e) => ExpressionContext d (NonEmpty e) where
  overExpression = traverse . overExpression

instance (ExpressionContext d d) => ExpressionContext d (Map e d) where
  overExpression = traverse . overExpression

instance (ExpressionContext d e) => ExpressionContext d (Maybe e) where
  overExpression = traverse . overExpression

-- instance ExpressionContext (Expression t) (CompiledMatch t) where
--  overExpression f =
--    \case
--      CompiledMatchClauses es ->
--        CompiledMatchClauses <$> overExpression f es
--      CompiledMatchExpression e ->
--        CompiledMatchExpression <$> overExpression f e

instance ExpressionContext (Expression a t) (Binding Expression a t) where
  overExpression f =
    \case
      BPattern a p e ->
        BPattern a p <$> overExpression f e
      BFunction{} ->
        error "TODO"

instance (ExpressionContext d (Expression a t)) => ExpressionContext d (Clause Expression a t) where
  overExpression f =
    \case
      EClause a p cs ->
        EClause a p <$> overExpression f cs

instance (ExpressionContext d (Expression a t)) => ExpressionContext d (Choice Expression a t) where
  overExpression f =
    \case
      CPlain a gs e ->
        CPlain a <$> overExpression f gs <*> overExpression f e
      CLambda a ps gs e ->
        CLambda a ps <$> overExpression f gs <*> overExpression f e

instance (ExpressionContext d (Expression a t)) => ExpressionContext d (Guard Expression a t) where
  overExpression f =
    \case
      CGuard e ->
        CGuard <$> overExpression f e
