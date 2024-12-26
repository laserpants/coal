{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Type.HasKind (HasKind (..)) where

import Noll.Language.Type (Type (..))
import Noll.Language.Type.Index (TypeIndex (..))
import Noll.Language.Type.Kind (Kind (..))

class HasKind p k where
  kindOf :: k -> Kind p

instance HasKind p (Kind p) where
  kindOf = id

instance HasKind p (TypeIndex (Kind p)) where
  kindOf =
    \case
      TypeIndex k _ ->
        kindOf k

instance (HasKind p (o (Kind p))) => HasKind p (Type o (Kind p)) where
  kindOf =
    \case
      TAlias _ _ t -> do
        kindOf t
      TApplication k _ _ ->
        kindOf k
      TArrow{} ->
        KType
      TIntrinsic{} ->
        KType
      TRow{} ->
        KRow
      TVariable t ->
        kindOf t
      TConstructor k _ ->
        kindOf k
