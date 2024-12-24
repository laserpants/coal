{-# LANGUAGE StrictData #-}

module Noll.Language.Kind.Index (KindIndex (..)) where

newtype KindIndex = KindIndex Int
  deriving (Show, Eq, Ord, Read)
