{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Common.Label (Label (..), setLabelName) where

import Data.Data (Data, Typeable)
import Extra (Name)

data Label t = Label {labelTag :: t, labelName :: Name}
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)

{-# INLINE setLabelName #-}
setLabelName :: Name -> Label t -> Label t
setLabelName name (Label t _) = Label t name
