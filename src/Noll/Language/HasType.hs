{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.HasType (HasType (..)) where

import Noll.Label (Label (..))
import Noll.Language.Expression (Expression (..))
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

instance HasType o k (Pattern (Type o k)) where
  typeOf =
    \case
      PVariable t ->
        typeOf t
      PConstructor t _ ->
        typeOf t

instance HasType o k (Expression (Type o k)) where
  typeOf =
    \case
      ELiteral t ->
        typeOf t
      EConstructor t ->
        typeOf t
      EVariable t ->
        typeOf t
      EApplication t _ _ ->
        typeOf t
      EIf _ _ t ->
        typeOf t
      ELet _ t ->
        typeOf t
      ELambda ts t ->
        foldType (typeOf t) (typeOf <$> ts)
      EBinaryOperator (t, _) ->
        typeOf t
      EUnaryOperator (t, _) ->
        typeOf t
      ERecord t _ _ ->
        typeOf t
      EListCons t _ _ ->
        typeOf t
      EListLiteral t _ ->
        typeOf t
