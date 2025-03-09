{-# LANGUAGE StrictData #-}

module Noll.Core.Language.Prim (Prim (..)) where

import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)

-- | Core language primitives
data Prim
  = -- | Unit value
    PUnit
  | -- | Booleans
    PBool Bool
  | -- | 32-bit integers
    PInt32 Int32
  | -- | 64-bit integers
    PInt64 Int64
  | -- | Single-precision floating-point numbers
    PFloat Float
  | -- | Double-precision floating-point numbers
    PDouble Double
  | -- | Single characters
    PChar Int32
  | -- | UTF-8 encoded strings
    PString ByteString
  deriving (Show, Eq, Ord, Read)
