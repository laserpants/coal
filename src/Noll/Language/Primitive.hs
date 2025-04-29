{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Primitive (Primitive (..)) where

import Data.Data (Data, Typeable)
import Data.ByteString (ByteString)
import GHC.Int (Int32, Int64)

-- | Language primitives
data Primitive
  = -- | Unit value
    LUnit
  | -- | Booleans
    LBool Bool
  | -- | 32-bit integers
    LInt32 Int32
  | -- | 64-bit integers
    LInt64 Int64
  | -- | Arbitrary precision integers
    LBignum Integer
  | -- | Single-precision floating point numbers
    LFloat Float
  | -- | Double-precision floating point numbers
    LDouble Double
  | -- | Unicode character type
    LChar Int32
  | -- | Strings
    LString ByteString
  deriving (Show, Eq, Ord, Read, Data, Typeable)
