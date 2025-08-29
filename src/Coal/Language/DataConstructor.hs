{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.DataConstructor (DataConstructor (..)) where

import Coal.Language.Type.Scheme (Scheme (..))
import Data.Data (Data, Typeable)
import Extra (Name)
import GHC.Generics (Generic)

-- | Data constructor
data DataConstructor o k t = DataConstructor
  { constructorName :: Name
  , constructorArity :: Int
  , constructorScheme :: Scheme o k t
  }
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable, Generic)
