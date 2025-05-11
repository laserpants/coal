{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Constructor (Constructor (..)) where

import Data.Data (Data, Typeable)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Lang.Utils (Name)
import Noll.Language.Type.Scheme (Scheme (..))

-- | Data constructor
data Constructor o k t = Constructor
  { constructorName :: Name
  , constructorArity :: Int
  , constructorScheme :: Scheme o k t
  }
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable, Generic)

instance (Hashable k, Hashable (o k), Hashable t) => Hashable (Constructor o k t)
