{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.HasType (HasType (..)) where

import Noll.Label (Label (..))
import Noll.Language.Expression (Expression (..))
import Noll.Language.Expression.Choice (Choice (..), Guard (..))
import Noll.Language.Pattern (Pattern (..))
import Noll.Language.Primitive (Primitive (..))
import Noll.Language.Type (Type (..), foldType)
import Noll.Language.Type.Intrinsic (Intrinsic (..))

class HasType o k t where
  typeOf :: t -> Type o k

instance HasType o k (Type o k) where
  typeOf = id

instance (HasType o k t) => HasType o k (Label t) where
  typeOf =
    \case
      Label t _ ->
        typeOf t

instance HasType o k Primitive where
  typeOf =
    \case
      LUnit ->
        TIntrinsic IUnit
      LBool{} ->
        TIntrinsic IBool
      LInt32{} ->
        TIntrinsic IInt32
      LInt64{} ->
        TIntrinsic IInt64
      LFloat{} ->
        TIntrinsic IFloat
      LDouble{} ->
        TIntrinsic IDouble
      LChar{} ->
        TIntrinsic IChar
      LString{} ->
        TIntrinsic IString

instance HasType o k (Pattern a (Type o k)) where
  typeOf =
    \case
      PAny _ t ->
        typeOf t
      PVariable _ t ->
        typeOf t
      PConstructor _ t _ ->
        typeOf t
      PLiteral _ t ->
        typeOf t
      PRecord _ t _ _ ->
        typeOf t
      PListCons _ t _ _ ->
        typeOf t
      PListLiteral _ t _ ->
        typeOf t

instance HasType o k (Guard Expression a (Type o k)) where
  typeOf =
    \case
      CGuard e ->
        typeOf e

instance HasType o k (Expression a (Type o k)) where
  typeOf =
    \case
      EAnnotation _ t ->
        typeOf t
      ELiteral _ t ->
        typeOf t
      EConstructor _ t ->
        typeOf t
      EVariable _ t ->
        typeOf t
      EApplication _ t _ _ ->
        typeOf t
      EIf _ t _ _ _ ->
        typeOf t
      ELet _ _ t ->
        typeOf t
      ERecursiveLet _ _ _ t ->
        typeOf t
      ELambda _ ts t ->
        foldType (typeOf t) (typeOf <$> ts)
      EBinaryOperator _ (t, _) ->
        typeOf t
      EUnaryOperator _ (t, _) ->
        typeOf t
      ERecord _ t _ _ ->
        typeOf t
      EListCons _ t _ _ ->
        typeOf t
      EListLiteral _ t _ ->
        typeOf t
