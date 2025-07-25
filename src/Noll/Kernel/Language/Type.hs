{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StrictData #-}

module Noll.Kernel.Language.Type (Type (..)) where

import Data.Data (Data, Typeable)
import Extra (Name)

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
  deriving (Show, Eq, Ord, Read, Data, Typeable)
