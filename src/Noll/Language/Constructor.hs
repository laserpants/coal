{-# LANGUAGE StrictData #-}

module Noll.Language.Constructor (Constructor (..)) where

import Noll.Language.Type.Scheme (Scheme (..))
import Noll.Utils (Name)

-- | Data constructor
data Constructor o k t = Constructor
  { constructorName :: Name
  , constructorArity :: Int
  , constructorScheme :: Scheme o k t
  }
  deriving (Show, Eq, Ord, Read)
