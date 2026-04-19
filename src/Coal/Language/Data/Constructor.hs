{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Language.Data.Constructor

Data constructor representation.
-}
module Coal.Language.Data.Constructor (DataConstructor (..)) where

import Coal.Language.Type.Scheme (Scheme (..))
import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import Extras (Name)
import GHC.Generics (Generic)

-- | Data constructor
data DataConstructor o k t = DataConstructor
  { constructorName :: Name
  , constructorArity :: Int
  , constructorScheme :: Scheme o k t
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Foldable
    , Data
    , Typeable
    , Generic
    )

instance (Binary t, Binary (o k)) => Binary (DataConstructor o k t)
