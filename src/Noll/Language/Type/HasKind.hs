{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Type.HasKind (HasKind (..)) where

import Noll.Language.Type (Type (..), TypeIndex (..))
import Noll.Language.Type.Kind (Kind (..))

class HasKind u k where
  kindOf :: k -> Kind u

instance HasKind u (Kind u) where
  kindOf = id

instance HasKind u (TypeIndex (Kind u)) where
  kindOf =
    \case
      TypeIndex k _ ->
        kindOf k

instance (HasKind u (o (Kind u))) => HasKind u (Type o (Kind u)) where
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
