{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

{- |
Primitive literal values.

Defines the set of primitive values supported by the language:

  * Unit (@()@)
  * Booleans (@bool@)
  * Fixed-width integers (@int32@, @int64@)
  * Arbitrary-precision integers (@bignum@)
  * Floating-point numbers (@float@, @double@)
  * Characters (Unicode code points, @char@)
  * Strings (UTF-8 encoded, @string@)

Primitives are always fully evaluated—they cannot contain unevaluated
subexpressions.
-}
module Coal.Kernel.Language.Prim (Prim (..)) where

import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import GHC.Generics (Generic)

{- | Core language primitive literals.

Each constructor represents a fully evaluated primitive value. Strings are
stored as raw 'ByteString' (UTF-8 encoded) for efficient manipulation and
interoperability with C runtime functions.
-}
data Prim
  = -- | Unit value
    PUnit
  | -- | Booleans
    PBool Bool
  | -- | 32-bit integers
    PInt32 Int32
  | -- | 64-bit integers
    PInt64 Int64
  | -- | Arbitrary precision integers
    PBignum Integer
  | -- | Single-precision floating-point numbers
    PFloat Float
  | -- | Double-precision floating-point numbers
    PDouble Double
  | -- | Single character (Unicode code point)
    PChar Int32
  | -- | UTF-8 encoded strings
    PString ByteString
  deriving (Show, Eq, Ord, Read, Generic)

instance Binary Prim
