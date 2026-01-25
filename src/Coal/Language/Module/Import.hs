{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Import (Import (..)) where

import Data.Data (Data, Typeable)
import Extras (Name)

data Import a
  = NameImport a Name
  | TypeImport a Name [Name]
  --  | TraitImport a Name [Name]
  deriving (Show, Eq, Ord, Read, Data, Typeable)
