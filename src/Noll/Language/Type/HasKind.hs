{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Type.HasKind (HasKind (..)) where

import Noll.Language.Type (Type (..), TypeIndex (..))
import Noll.Language.Type.Kind (Kind (..))

class HasKind k where
  kindOf :: k -> Kind

instance HasKind Kind where
  kindOf = id

instance HasKind (TypeIndex Kind) where
  kindOf =
    \case
      TypeIndex k _ ->
        kindOf k

instance (HasKind (o Kind)) => HasKind (Type o Kind) where
  kindOf =
    \case
      TAlias _ _ k -> do
        kindOf k
      TApplication k _ _ ->
        kindOf k
      TArrow{} ->
        KType
      TIntrinsic{} ->
        KType
      TRow{} ->
        KRow
      TVariable k ->
        kindOf k
      TConstructor k _ ->
        kindOf k
