{-# LANGUAGE StrictData #-}

module Noll.Language.Primitive (Primitive (..)) where

import Data.Text (Text)
import GHC.Int (Int32, Int64)

-- | Language primitives
data Primitive
  = -- | Unit value
    AUnit
  | -- | Booleans
    ABool Bool
  | -- | 32-bit integers
    AInt32 Int32
  | -- | 64-bit integers
    AInt64 Int64
  | -- | Single-precision floating point numbers
    AFloat Float
  | -- | Double-precision floating point numbers
    ADouble Double
  | -- | Unicode character type
    AChar Int32
  | -- | Strings
    AString Text
  deriving (Show, Eq, Ord, Read)
