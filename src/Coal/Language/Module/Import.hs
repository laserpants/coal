{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Language.Module.Import

Module import declarations for names and types.
-}
module Coal.Language.Module.Import (Import (..)) where

import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import Extras (Name)
import GHC.Generics (Generic)

data Import a
  = NameImport a Name
  | TypeImport a Name [Name]
  deriving (Show, Eq, Ord, Read, Data, Typeable, Generic)

instance (Binary a) => Binary (Import a)
