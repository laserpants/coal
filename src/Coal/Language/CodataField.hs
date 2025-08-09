{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.CodataField where

import Coal.Language.Type.Scheme (Scheme (..))
import Data.Data (Data, Typeable)
import Extra (Name)
import GHC.Generics (Generic)

data CodataField o k t = CodataField
  { codataFieldName :: Name
  , codataFieldType :: t
  , codataFieldCotype :: Scheme o k t
  }
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable, Generic)
