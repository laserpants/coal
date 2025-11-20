{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Import (Import (..)) where

import Data.Data (Data, Typeable)
import Extras (Name)

data Import a
  = ImportName a Name
  | ImportType a Name [Name]
  | ImportCotype a Name [Name]
  | ImportTrait a Name [Name]
  deriving (Show, Eq, Ord, Read, Data, Typeable)
