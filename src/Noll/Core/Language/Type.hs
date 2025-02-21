{-# LANGUAGE StrictData #-}

module Noll.Core.Language.Type (Type (..)) where

import Noll.Utils (Name)

-- | Core language types
data Type
  = -- | Type constructor
    TCon Name [Type]
  | -- | Opaque type
    TOpq
  | -- | Row extension
    RExt Name Type Type
  | -- | Empty row
    RNil
  deriving (Show, Eq, Ord, Read)
