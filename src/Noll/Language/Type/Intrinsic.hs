{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Type.Intrinsic (Intrinsic (..)) where

import Data.Data (Data, Typeable)

-- | Built-in types
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
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)
