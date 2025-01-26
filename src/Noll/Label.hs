{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Label (Label (..), labelName) where

import Noll.Utils (Name)

data Label t = Label t Name
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Functor
    , Foldable
    , Traversable
    )

{-# INLINE labelName #-}
labelName :: Label t -> Name
labelName (Label _ name) = name
