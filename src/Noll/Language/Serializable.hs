{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Language.Serializable (Serializable (..)) where

import Data.Text (Text)
import Noll.Common.List1 (List1, fromList1)
import Noll.Language.Trait
import Noll.Language.Type
import Noll.Language.Type.Intrinsic
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
      ITuple ts ->
        "Tuple" <> parenthesized ts
      IList t ->
        "List" <> parenthesized t
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
      TRow{} ->
        "TODO"
      TVariable t ->
        "Variable" <> parenthesized t
      _ ->
        error "Not implemented"

instance (Serializable s) => Serializable (Trait s) where
  serialize =
    \case
      Trait name t ->
        name <> parenthesized t
