{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Type.Intrinsic (Intrinsic (..)) where

-- | Built-in type constructors
data Intrinsic t
  = IBool
  | IChar
  | IDouble
  | IFloat
  | IInt32
  | IInt64
  | IList t
  | INat
  | IOption t
  | IRecord t
  | IResult t
  | IString
  | ITuple [t]
  | IUnit
  | IVoid
  deriving (Show, Eq, Ord, Read, Functor)
