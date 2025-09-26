{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.CodataAccessor (CodataAccessor (..)) where

import Coal.Language.Type.Scheme (Scheme (..))
import Data.Data (Data, Typeable)
import Extra (Name)
import GHC.Generics (Generic)

{- | Codata field accessor
e.g., Head : Stream<a> -> a
-}
data CodataAccessor o k t = CodataAccessor
  { codataAccessorName :: Name
  , codataAccessorScheme :: Scheme o k t
  }
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable, Generic)
