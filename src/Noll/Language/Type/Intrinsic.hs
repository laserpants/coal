{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Type.Intrinsic (Intrinsic (..)) where

-- | Built-in type constructors
data Intrinsic t
  = Bool
  | Char
  | Double
  | Float
  | Int32
  | Int64
  | List t
  | Nat
  | Option t
  | Record t
  | Result t
  | String
  | Tuple [t]
  | Unit
  | Void
  deriving (Show, Eq, Ord, Read, Functor)
