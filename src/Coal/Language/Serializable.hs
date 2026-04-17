-- +
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Language.Serializable (
  Serializable (..),
  instanceLabel,
  dictionaryLabel,
) where

import Coal.Language.Trait (Trait (..), traitName)
import Coal.Language.Type (Parameter (..), Type (..), TypeIndex (..))
import Coal.Language.Type.Intrinsic (Intrinsic (..))
import Coal.Language.Type.Row (Row (..))
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Text (Text)
import Extras (Name)
import TextShow (showt)

class Serializable s where
  serialize :: s -> Text

instance Serializable Text where
  serialize = id

instance Serializable Int where
  serialize = showt

{-# INLINE parenthesized #-}
parenthesized :: (Serializable s) => s -> Text
parenthesized s = "(" <> serialize s <> ")"

instance Serializable (TypeIndex k) where
  serialize =
    \case
      TypeIndex _ n ->
        "TypeIndex" <> parenthesized n

instance Serializable (Parameter k) where
  serialize =
    \case
      Parameter _ n ->
        "Parameter" <> parenthesized n

instance (Serializable s) => Serializable (NonEmpty s) where
  serialize = serialize . toList

instance (Serializable s) => Serializable [s] where
  serialize =
    \case
      [] -> ""
      [t] -> serialize t
      (t : ts) -> serialize t <> "," <> serialize ts

instance (Serializable s1, Serializable s2) => Serializable (s1, s2) where
  serialize (a1, a2) =
    "(" <> serialize a1 <> "," <> serialize a2 <> ")"

instance (Serializable s1, Serializable s2, Serializable s3) => Serializable (s1, s2, s3) where
  serialize (a1, a2, a3) =
    "(" <> serialize a1 <> "," <> serialize a2 <> "," <> serialize a3 <> ")"

instance Serializable Intrinsic where
  serialize =
    \case
      IBool ->
        "IBool"
      IChar ->
        "IChar"
      IDouble ->
        "IDouble"
      IFloat ->
        "IFloat"
      IInt32 ->
        "IInt32"
      IInt64 ->
        "IInt64"
      IBignum ->
        "IBignum"
      IString ->
        "IString"
      INat ->
        "INat"
      IUnit ->
        "IUnit"
      IVoid ->
        "IVoid"

instance (Serializable (o k)) => Serializable (Type o k) where
  serialize =
    \case
      TApplication _ t1 ts ->
        "TApplication" <> parenthesized t1 <> parenthesized ts
      TArrow t1 t2 ->
        "TArrow" <> parenthesized [t1, t2]
      TConstructor _ name ->
        "TConstructor" <> parenthesized name
      TIntrinsic t ->
        "TIntrinsic" <> parenthesized t
      TRecord t ->
        "TRecord" <> parenthesized t
      TRow r ->
        "TRow" <> parenthesized r
      TVariable t ->
        "TVariable" <> parenthesized t
      TAlias name ts t ->
        "TAlias" <> parenthesized (name, ts, t)

instance (Serializable (o k), Serializable t) => Serializable (Row o k t) where
  serialize =
    \case
      RNil ->
        "RNil"
      RVariable r ->
        "RVariable" <> parenthesized r
      RExtend n t r ->
        "RExtend" <> parenthesized (n, t, r)

instance (Serializable s) => Serializable (Trait s) where
  serialize =
    \case
      Trait name t ->
        name <> parenthesized t

instanceLabel :: (Serializable t) => Trait t -> Name -> Name
instanceLabel trait name = name <> "__$impl_" <> serialize trait

dictionaryLabel :: (Serializable t) => Trait t -> Name
dictionaryLabel trait = "$d_" <> instanceLabel trait (traitName trait)
