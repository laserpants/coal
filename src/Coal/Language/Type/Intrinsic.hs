{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Type.Intrinsic (Intrinsic (..)) where

import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

-- | Built-in types
data Intrinsic
  = IBool
  | IChar
  | IDouble
  | IFloat
  | IInt32
  | IInt64
  | IBignum
  | INat
  | IString
  | IUnit
  | IVoid
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Data
    , Typeable
    , Generic
    )

instance Binary Intrinsic
