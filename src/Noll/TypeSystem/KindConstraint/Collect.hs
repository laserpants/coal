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
import qualified Data.Text as Text
import Debug.Trace
import Noll.Label (Label (..))
import Noll.Language (
  Binding (..),
  Expression (..),
  HasKind (..),
  Kind (..),
  KindIndex (..),
  Pattern (..),
  Type (..),
  TypeIndex (..),
  foldKind,
 )
import Noll.Library.Environment (Environment (..))
import qualified Noll.Library.Environment as Environment
import Noll.TypeSystem.KindConstraint (KindConstraint (..), KindConstraintMetadata (..))
import Noll.Utils (Dictionary, forM_, traverse_)

type KindConstraintsMonad c k = RWS (Environment k) [KindConstraint c k] ()

newtype KindConstraints c k a = KindConstraints {constraintsMonad :: KindConstraintsMonad c k a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (Environment k)
    , MonadWriter [KindConstraint c k]
    , MonadState ()
    , MonadRWS (Environment k) [KindConstraint c k] ()
    )

{-# INLINE runCollectKindConstraints #-}
runCollectKindConstraints :: Environment k -> KindConstraints c k a -> [KindConstraint c k]
runCollectKindConstraints d cs = snd (execRWS (constraintsMonad cs) d ())

collectConstraintsInType :: Type TypeIndex (Kind KindIndex) -> KindConstraints KindConstraintMetadata (Kind KindIndex) ()
collectConstraintsInType =
  \case
    TApplication k t ts -> do
      collectConstraintsInType t
      traverse_ collectConstraintsInType ts
      tell [KindEquality KindConstraintMetadata (kindOf t) (foldKind k (kindOf <$> ts))]
    TArrow t1 t2 -> do
      collectConstraintsInType t1
      collectConstraintsInType t2
    TIntrinsic t -> do
      traverse_ collectConstraintsInType t
    TRow row ->
      -- TODO
      undefined
    TAlias _ _ t ->
      collectConstraintsInType t
    TConstructor k name -> do
      env <- ask
      case Environment.lookup name env of
        Nothing ->
          error ("No type constructor '" <> Text.unpack name <> "'")
        Just k1 ->
          tell [KindEquality KindConstraintMetadata k k1]
      pure ()
    TVariable (TypeIndex k _) ->
      pure ()

collectKindConstraints :: Expression a (Type TypeIndex (Kind KindIndex)) -> KindConstraints KindConstraintMetadata (Kind KindIndex) ()
collectKindConstraints =
  \case
    EConstructor _ (Label t name) -> do
      collectConstraintsInType t
    EVariable _ (Label t _) -> do
      tell [KindEquality KindConstraintMetadata (kindOf t) KType]
      collectConstraintsInType t
    ELambda _ _ e -> do
      collectKindConstraints e
    ELet _ gs e1 -> do
      forM_ gs $
        \case
          BPattern _ (PVariable _ (Label t _)) e -> do
            collectConstraintsInType t
            collectKindConstraints e
      collectKindConstraints e1
    EIf _ t e1 e2 e3 -> do
      collectConstraintsInType t
      collectKindConstraints e1
      collectKindConstraints e2
      collectKindConstraints e3
    EApplication _ t e1 es -> do
      collectConstraintsInType t
      collectKindConstraints e1
      traverse_ collectKindConstraints es
    ELiteral{} ->
      pure ()
    EMatch _ t es cs -> do
      traverse_ collectKindConstraints es
      -- TODO
      pure ()
