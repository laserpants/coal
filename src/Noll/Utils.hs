{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Utils where -- (lexOrderRank, hashed) where

import Data.Char (ord)
import Data.Hashable (Hashable, hash)
import Data.Text (Text)
import Lang.Common.List1 (List1, fromList1)
import Noll.Language.Trait
import Noll.Language.Type
import Noll.Language.Type.Intrinsic
import Numeric (showHex)
import TextShow

import qualified Data.Text as Text

{-# INLINE inCharRange #-}
inCharRange :: Int -> (Char, Char) -> Bool
inCharRange n (a, b) = n >= ord a && n <= ord b

lexOrderRank :: Text -> Int
lexOrderRank text
  | Text.null text =
      error "Empty string"
  | otherwise =
      snd (Text.foldr f (0, 0) text) - 1
 where
  f :: Char -> (Int, Int) -> (Int, Int)
  f c (m, n) = (m + 1, n + (36 ^ m) + g (ord c))
  g n
    | not (n `inCharRange` ('a', 'z') || n `inCharRange` ('0', '9')) =
        error "Invalid character"
    | n >= ord 'a' = n - ord 'a'
    | otherwise = n - 22

hashed :: (Hashable a) => a -> Text
hashed t = Text.pack (showHex (fromIntegral (hash t) :: Word) "")

class Serializable s where
  serialize :: s -> Text

instance Serializable (TypeIndex k) where
  serialize =
    \case
      TypeIndex _ n ->
        "TypeIndex" <> "(" <> showt n <> ")"

instance Serializable (Parameter k) where
  serialize =
    \case
      Parameter _ n ->
        "Parameter" <> "(" <> n <> ")"

instance (Serializable s) => Serializable (List1 s) where
  serialize = serialize . fromList1

instance (Serializable s) => Serializable [s] where
  serialize =
    \case
      [] -> ""
      [t] -> serialize t
      (t : ts) -> serialize t <> "," <> serialize ts

instance (Show s, Serializable s) => Serializable (Intrinsic s) where
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
        "Tuple" <> "(" <> serialize ts <> ")"
      IList t ->
        "List" <> "(" <> serialize t <> ")"
      _ ->
        error "Not implemented"

instance (Show k, Show (s k), Serializable (s k)) => Serializable (Type s k) where
  serialize =
    \case
      TApplication _ t1 ts ->
        "Application" <> "(" <> serialize t1 <> ")(" <> serialize ts <> ")"
      TArrow t1 t2 ->
        "Arrow" <> "(" <> serialize [t1, t2] <> ")"
      TConstructor _ name ->
        "Constructor" <> "(" <> name <> ")"
      TIntrinsic t ->
        "Intrinsic" <> "(" <> serialize t <> ")"
      TRow{} ->
        "TODO"
      TVariable t ->
        "Variable" <> "(" <> serialize t <> ")"
      _ ->
        error "Not implemented"

instance (Serializable s) => Serializable (Trait s) where
  serialize =
    \case
      Trait name t ->
        name <> "(" <> serialize t <> ")"
