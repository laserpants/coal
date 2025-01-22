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

--        EApplication t e1 es ->
--          EApplication t <$> overExpression f e1 <*> overExpression f es
--        EBlock es ->
--          EBlock <$> overExpression f es
--        EBlockAssign t name e ->
--          EBlockAssign t name <$> overExpression f e
--        EIf e1 e2 e3 ->
--          EIf
--            <$> overExpression f e1
--            <*> overExpression f e2
--            <*> overExpression f e3
--        EListLiteral t es ->
--          EListLiteral t <$> overExpression f es
--        ETuple es ->
--          ETuple <$> overExpression f es
--        ESelect t name e ->
--          ESelect t name <$> overExpression f e
--        ELambda ps e ->
--          ELambda ps <$> overExpression f e
--        ELet bs e ->
--          ELet <$> overExpression f bs <*> overExpression f e
--        EListCons t e1 e2 ->
--          EListCons t <$> overExpression f e1 <*> overExpression f e2
--        --    --    ECompiledMatch e es ->
--        --    --      ECompiledMatch (overExpr f e) (overExprClause f <$> es)
--        EMatch t es cs bcs ->
--          EMatch t <$> overExpression f es <*> overExpression f cs <*> overExpression f bcs
--        EFold t es cs me ->
--          EFold t <$> overExpression f es <*> overExpression f cs <*> overExpression f me
--        ELambdaMatch cs ->
--          ELambdaMatch <$> overExpression f cs
--        ERecord t d me ->
--          ERecord t <$> overExpression f d <*> overExpression f me
--        e ->
--          -- TOOO
--          pure e

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

-- instance (ExpressionContext d (e t)) => ExpressionContext d (Clause e t) where
--  overExpression f =
--    \case
--      EClause ps cs ->
--        EClause ps <$> overExpression f cs
--
-- instance ExpressionContext d (CompiledClause e t) where
--  overExpression f =
--    error "TODO"
--
-- instance (ExpressionContext d (e t)) => ExpressionContext d (Choice e t) where
--  overExpression f =
--    \case
--      CPlain gs e ->
--        CPlain <$> overExpression f gs <*> overExpression f e
--      CLambda ps gs e ->
--        CLambda ps <$> overExpression f gs <*> overExpression f e
--
-- instance (ExpressionContext d (e t)) => ExpressionContext d (Guard e t) where
--  overExpression f =
--    \case
--      CGuard e ->
--        CGuard <$> overExpression f e
