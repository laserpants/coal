{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.HasKind (HasKind (..), foldKindOf) where

import Coal.Language.Type (Parameter (..), Type (..), TypeIndex (..))
import Coal.Language.Type.Kind (Kind (..), foldKind)
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

{-# INLINE foldKindOf #-}
foldKindOf :: (HasKind k, HasKind l, Functor f, Foldable f) => l -> f k -> Kind
foldKindOf a as = foldKind (kindOf a) (kindOf <$> as)
