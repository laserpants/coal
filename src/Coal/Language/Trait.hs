-- +
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Trait (Trait (..), Qualified (..)) where

import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import Extras (Name)
import GHC.Generics (Generic)

-- | Standalone type trait
data Trait t = Trait
  { traitName :: Name
  , traitType :: t
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Functor
    , Foldable
    , Traversable
    , Data
    , Typeable
    , Generic
    )

instance (Binary t) => Binary (Trait t)

-- | Qualified type
data Qualified t = With [Trait t] t
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Functor
    , Foldable
    , Traversable
    , Data
    , Typeable
    , Generic
    )

instance (Binary t) => Binary (Qualified t)
