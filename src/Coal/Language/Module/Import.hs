-- +
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Import (Import (..)) where

import Data.Binary
import Data.Data (Data, Typeable)
import Extras (Name)
import GHC.Generics (Generic)

data Import a
  = NameImport a Name
  | TypeImport a Name [Name]
  deriving (Show, Eq, Ord, Read, Data, Typeable, Generic)

instance (Binary a) => Binary (Import a)
