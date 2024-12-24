{-# LANGUAGE StrictData #-}

module Noll.Language.Type.Kind.Index (KindIndex (..)) where

newtype KindIndex = KindIndex Int
  deriving (Show, Eq, Ord, Read)
