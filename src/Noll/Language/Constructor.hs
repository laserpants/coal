{-# LANGUAGE StrictData #-}

module Noll.Language.Constructor (Constructor (..)) where

import Noll.Language.Type.Scheme (Scheme (..))
import Noll.Utils (Name)

data Constructor o k t = Constructor
  { constructorName :: Name
  , constructorScheme :: Scheme o k t
  }
  deriving (Show, Eq, Ord, Read)
