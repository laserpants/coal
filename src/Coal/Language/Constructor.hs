{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Constructor (Constructor (..)) where

import Coal.Language.Type.Scheme (Scheme (..))
import Data.Data (Data, Typeable)
import Extra (Name)
import GHC.Generics (Generic)

-- | Data constructor
data Constructor o k t = Constructor
  { constructorName :: Name
  , constructorArity :: Int
  , constructorScheme :: Scheme o k t
  }
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable, Generic)
