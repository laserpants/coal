{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Language.Serializable (Serializable (..)) where

import Coal.Common.List1 (List1, fromList1)
import Coal.Language.Trait
import Coal.Language.Type
import Coal.Language.Type.Intrinsic
import Coal.Language.Type.Row
import Data.Text (Text)
import TextShow (showt)

class Serializable s where
  serialize :: s -> Text

instance Serializable Text where
  serialize = id

instance Serializable Int where
  serialize = showt

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

instance (Serializable s) => Serializable (List1 s) where
  serialize = serialize . fromList1

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

instance (Serializable s) => Serializable (Intrinsic s) where
  serialize =
    \case
      IBool ->
        "Bool"
      IChar ->
        "Char"
      IDouble ->
        "Double"
      IFloat ->
        "Float"
      IInt32 ->
        "Int32"
      IInt64 ->
        "Int64"
      IBignum ->
        "Bignum"
      IString ->
        "String"
      INat ->
        "Nat"
      IRecord t ->
        "Record" <> parenthesized t
      _ ->
        error "Not implemented"

instance (Serializable (s k)) => Serializable (Type s k) where
  serialize =
    \case
      TApplication _ t1 ts ->
        "Application" <> parenthesized t1 <> parenthesized ts
      TArrow t1 t2 ->
        "Arrow" <> parenthesized [t1, t2]
      TConstructor _ name ->
        "Constructor" <> parenthesized name
      TIntrinsic t ->
        "Intrinsic" <> parenthesized t
      TRow r ->
        "Row" <> parenthesized r
      TVariable t ->
        "Variable" <> parenthesized t
      _ ->
        error "Not implemented"

instance (Serializable (o k), Serializable t) => Serializable (Row o k t) where
  serialize =
    \case
      RNil ->
        "RowNil"
      RVariable r ->
        "RowVariable" <> parenthesized r
      RExtend n t r ->
        "RowExtend" <> parenthesized (n, t, r)

instance (Serializable s) => Serializable (Trait s) where
  serialize =
    \case
      Trait name t ->
        name <> parenthesized t
