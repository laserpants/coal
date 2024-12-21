{-# LANGUAGE StrictData #-}

module Noll.Language.Primitive (Primitive (..)) where

import Data.Text (Text)
import GHC.Int (Int32, Int64)

-- | Language primitives
data Primitive
  = -- | Unit value
    Unit
  | -- | Booleans
    Bool Bool
  | -- | 32-bit integers
    Int32 Int32
  | -- | 64-bit integers
    Int64 Int64
  | -- | Single-precision floating point numbers
    Float Float
  | -- | Double-precision floating point numbers
    Double Double
  | -- | Unicode character type
    Char Int32
  | -- | Strings
    String Text
  deriving (Show, Eq, Ord, Read)
