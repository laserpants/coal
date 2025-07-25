{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.HasKind (HasKind (..)) where

import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (universeBi)
import Coal.Language.Type (Type (..), TypeIndex (..))
import Coal.Language.Type.Kind (Kind (..))

class HasKind k where
  kindOf :: k -> Kind

instance HasKind Kind where
  kindOf = id

instance HasKind (TypeIndex Kind) where
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
      k ->
        head (universeBi k)
