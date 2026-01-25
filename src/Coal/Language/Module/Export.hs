{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Export (Export (..)) where

import Data.Data (Data, Typeable)
import Extras (Name)

data Export a
  = NameExport a Name
  | TypeExport a Name [Name]
  | ExportAll
  deriving (Show, Eq, Ord, Read, Data, Typeable)
