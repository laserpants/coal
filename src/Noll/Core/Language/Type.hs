{-# LANGUAGE StrictData #-}

module Noll.Core.Language.Type where

import Noll.Language (Name)

-- | Core language types
data Type
  = -- | Type constructor
    Con Name [Type]
  | -- | Opaque type
    Opq
  | -- | Row extension
    RExt Name Type Type
  | -- | Empty row
    RNil
  deriving (Show, Eq, Ord, Read)
