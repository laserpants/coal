{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.KindConstraint.Collect (
  collectKindConstraints,
) where

import Control.Monad (forM_)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Foldable (traverse_)
import Noll.Label (Label (..))
import Noll.Language.Expression (Expression (..))
import qualified Noll.Language.Expression as Expr
import qualified Noll.Language.Expression.Binding as Binding
import qualified Noll.Language.Pattern as Pattern
import Noll.Language.Type (Type (..))
import qualified Noll.Language.Type as Type
import Noll.Language.Type.HasKind (HasKind (..))
import Noll.Language.Type.Index (TypeIndex (..))
import Noll.Language.Type.Kind (Kind (..), foldKind)
import qualified Noll.Language.Type.Kind as Kind
import Noll.Language.Type.Kind.Index (KindIndex (..))
import Noll.TypeSystem.KindConstraint (KindConstraint (..))

collectConstraintsInType :: (MonadWriter [KindConstraint (Kind KindIndex)] m) => Type TypeIndex (Kind KindIndex) -> m ()
collectConstraintsInType =
  \case
    Type.Application k t ts -> do
      collectConstraintsInType t
      traverse_ collectConstraintsInType ts
      tell [KindEquality k (foldKind (kindOf t) (kindOf <$> ts))]
    Type.Arrow t1 t2 -> do
      collectConstraintsInType t1
      collectConstraintsInType t2
    Type.Intrinsic t -> do
      traverse collectConstraintsInType t
      pure ()
    Type.Row row ->
      -- TODO
      undefined
    Type.Alias _ _ t ->
      collectConstraintsInType t
    Type.Constructor k name ->
      -- TODO
      pure ()
    Type.Variable (TypeIndex k _) ->
      pure ()

collectKindConstraints :: (MonadWriter [KindConstraint (Kind KindIndex)] m) => Expression (Type TypeIndex (Kind KindIndex)) -> m ()
collectKindConstraints =
  \case
    Expr.Constructor (Label t name) -> do
      collectConstraintsInType t
    Expr.Variable (Label t _) -> do
      tell [KindEquality (kindOf t) Kind.Type]
      collectConstraintsInType t
    Expr.Lambda _ e -> do
      collectKindConstraints e
    Expr.Let gs e1 -> do
      forM_ gs $
        \case
          Binding.Pattern (Pattern.Variable (Label t _)) e -> do
            collectConstraintsInType t
            collectKindConstraints e
      collectKindConstraints e1
    Expr.If e1 e2 e3 -> do
      collectKindConstraints e1
      collectKindConstraints e2
      collectKindConstraints e3
    Expr.Application t e1 es -> do
      collectConstraintsInType t
      collectKindConstraints e1
      traverse_ collectKindConstraints es
    Expr.Literal{} ->
      pure ()
