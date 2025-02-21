{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Label (Label (..), labelName, setLabelName) where

import Data.Data (Data, Typeable)
import Noll.Utils (Name)

data Label t = Label t Name
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)

{-# INLINE labelName #-}
labelName :: Label t -> Name
labelName (Label _ name) = name

{-# INLINE setLabelName #-}
setLabelName :: Name -> Label t -> Label t
setLabelName name (Label t _) = Label t name
