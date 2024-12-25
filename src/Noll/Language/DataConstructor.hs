{-# LANGUAGE StrictData #-}

module Noll.Language.DataConstructor where

import Noll.Language.Type.Scheme (Scheme (..))
import Noll.Utils (Name)

data DataConstructor o k t = DataConstructor
  { constructorName :: Name
  , constructorScheme :: Scheme o k t
  }
  deriving (Show, Eq, Ord, Read)
