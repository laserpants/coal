{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.KindConstraint.Collect where

import Control.Monad.Writer (MonadWriter, tell)
import Noll.Label (Label (..))
import Noll.Language.Expression (Expression (..))
import qualified Noll.Language.Expression as Expr
import Noll.Language.Type (Type (..))
import qualified Noll.Language.Type as Type
import Noll.Language.Type.Index (TypeIndex (..))
import Noll.Language.Type.Kind (Kind (..), foldKind)
import qualified Noll.Language.Type.Kind as Kind
import Noll.Language.Type.Kind.Index (KindIndex (..))
import Noll.TypeSystem.KindConstraint (KindConstraint (..))

-- Assumption?
data X = X

hello :: (MonadWriter [KindConstraint (Kind KindIndex)] m) => Type TypeIndex (Kind KindIndex) -> m ()
hello =
  \case
    Type.Application k t ts -> do
      hello t
      traverse hello ts
      pure ()
    --      tell [KindEquality k (foldKind k1 (kindOf <$> ts))]
    Type.Arrow t1 t2 -> do
      hello t1
      hello t2
    Type.Intrinsic t -> do
      traverse hello t
      pure ()
    --    Type.Row row ->
    --      pure ()
    Type.Alias _ _ t ->
      hello t
    Type.Constructor k _ ->
      pure ()
    Type.Variable (TypeIndex k _) ->
      pure ()

-- TODO
collectKindConstraints :: (MonadWriter [KindConstraint (Kind KindIndex)] m) => Expression (Type TypeIndex (Kind KindIndex)) -> m [X]
collectKindConstraints =
  \case
    Expr.Constructor (Label t name) -> do
      hello t
      pure []
    Expr.Variable (Label t name) -> do
      hello t
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
      collectKindConstraints e1
      traverse collectKindConstraints es
      pure []
    Expr.Literal{} ->
      pure []
