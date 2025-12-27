{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.HasKind (HasKind (..)) where

import Coal.Language.Type (Parameter (..), Type (..), TypeIndex (..))
import Coal.Language.Type.Kind (Kind (..))
import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (universeBi)

class HasKind k where
  kindOf :: k -> Kind

instance HasKind Kind where
  kindOf = id

instance HasKind (TypeIndex Kind) where
  kindOf = head . universeBi

instance HasKind (Parameter Kind) where
  kindOf = head . universeBi

instance (Data (o Kind), Typeable o) => HasKind (Type o Kind) where
  kindOf =
    \case
      TRow{} ->
        KRow
      TArrow{} ->
        KType
      TIntrinsic{} ->
        KType
      TRecord{} ->
        KType
      k ->
        head (universeBi k)
