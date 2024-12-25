{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.KindConstraint.Collect where

import Noll.Label (Label (..))
import Noll.Language.Expression (Expression (..))
import qualified Noll.Language.Expression as Expr
import Noll.Language.Type (Type (..))
import qualified Noll.Language.Type as Type
import Noll.Language.Type.Index (TypeIndex (..))
import Noll.Language.Type.Kind (Kind (..))
import Noll.Language.Type.Kind.Index (KindIndex (..))
import Noll.TypeSystem.TypeConstraint.Assumption (Assumption (..))

-- Assumption?
data X = X

hello :: (Monad m) => Type TypeIndex (Kind KindIndex) -> m [X]
hello =
  \case
    Type.Application _ t ts ->
      undefined
    Type.Arrow t1 t2 ->
      undefined
    Type.Constructor{} ->
      undefined
    Type.Intrinsic{} ->
      undefined
    Type.Row row ->
      undefined
    Type.Variable t ->
      undefined
    Type.Alias _ _ t ->
      undefined

-- TODO
collectKindConstraints :: (Monad m) => Expression (Type TypeIndex (Kind KindIndex)) -> m [X]
collectKindConstraints =
  \case
    Expr.Constructor (Label t name) -> do
      pure []
    Expr.Variable (Label t name) -> do
      pure []
    Expr.Lambda ps e -> do
      pure []
    Expr.Let gs e1 -> do
      pure []
    Expr.If e1 e2 e3 -> do
      collectKindConstraints e1
      collectKindConstraints e2
      collectKindConstraints e3
      pure []
    Expr.Application t e1 es -> do
      hello t
      pure []
    Expr.Literal{} ->
      pure []
