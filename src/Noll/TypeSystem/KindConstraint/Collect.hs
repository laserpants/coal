{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.KindConstraint.Collect where

import Noll.Label (Label (..))
import Noll.Language.Expression (Expression (..))
import qualified Noll.Language.Expression as Expr
import Noll.Language.Type (Type (..))
import Noll.Language.Type.Index (TypeIndex (..))
import Noll.Language.Type.Kind (Kind (..))
import Noll.Language.Type.Kind.Index (KindIndex (..))
import Noll.TypeSystem.TypeConstraint.Assumption (Assumption (..))

collectKindConstraints :: (Monad m) => Expression (Type TypeIndex (Kind KindIndex)) -> m [Assumption (Type TypeIndex (Kind KindIndex))]
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
      pure []
    Expr.Application t e1 es -> do
      pure []
    Expr.Literal{} ->
      pure []
