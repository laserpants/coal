{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Type.HasKind (HasKind (..)) where

import Noll.Language.Type (Type (..), TypeIndex (..))
import Noll.Language.Type.Kind (Kind (..))

class HasKind o k where
  kindOf :: k -> Kind o

instance HasKind o (Kind o) where
  kindOf = id

instance HasKind o (TypeIndex (Kind o)) where
  kindOf =
    \case
      TypeIndex k _ ->
        kindOf k

instance (HasKind o (a (Kind o))) => HasKind o (Type a (Kind o)) where
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
