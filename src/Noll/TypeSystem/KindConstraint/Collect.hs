{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.KindConstraint.Collect (
  collectKindConstraints,
  runCollectKindConstraints,
) where

import Control.Monad.RWS (MonadRWS, MonadReader, MonadState, MonadWriter, RWS, ask, execRWS, tell)
import qualified Data.Map.Strict as Map
import Noll.Label (Label (..))
import Noll.Language (
  Expression,
  HasKind (..),
  Kind,
  KindIndex (..),
  Type,
  TypeIndex (..),
  foldKind,
 )
import qualified Noll.Language.Expression as Expr
import qualified Noll.Language.Expression.Binding as Binding
import qualified Noll.Language.Pattern as Pattern
import qualified Noll.Language.Type as Type
import qualified Noll.Language.Type.Kind as Kind
import Noll.TypeSystem.KindConstraint (KindConstraint (..))
import Noll.Utils (Dictionary (..), forM_, traverse_)

type KindConstraintsMonad k = RWS (Dictionary k) [KindConstraint k] ()

newtype KindConstraints k a = KindConstraints {constraintsMonad :: KindConstraintsMonad k a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (Dictionary k)
    , MonadWriter [KindConstraint k]
    , MonadState ()
    , MonadRWS (Dictionary k) [KindConstraint k] ()
    )

{-# INLINE runCollectKindConstraints #-}
runCollectKindConstraints :: Dictionary k -> KindConstraints k a -> [KindConstraint k]
runCollectKindConstraints d cs = snd (execRWS (constraintsMonad cs) d ())

collectConstraintsInType :: Type TypeIndex (Kind KindIndex) -> KindConstraints (Kind KindIndex) ()
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
    Type.Constructor k name -> do
      env <- ask
      case Map.lookup name env of
        Nothing ->
          error "TODO"
        Just k1 ->
          tell [KindEquality k k1]
      pure ()
    Type.Variable (TypeIndex k _) ->
      pure ()

collectKindConstraints :: Expression (Type TypeIndex (Kind KindIndex)) -> KindConstraints (Kind KindIndex) ()
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
