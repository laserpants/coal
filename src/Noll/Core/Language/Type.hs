{-# LANGUAGE StrictData #-}

module Noll.Core.Language.Type (Type (..)) where

import Noll.Utils (Name)

-- | Core language types
data Type
  = -- | Type constructor
    Con Name [Type]
  | -- | Opaque type
    Opaque
  | -- | Row extension
    RExt Name Type Type
  | -- | Empty row
    RNil
  deriving (Show, Eq, Ord, Read)
