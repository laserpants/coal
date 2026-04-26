{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Common.Label (Label (..), setLabelName) where

import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import Extras (Name)
import GHC.Generics (Generic)

data Label t = Label
  { labelTag :: t
  , labelName :: Name
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Functor
    , Foldable
    , Traversable
    , Data
    , Typeable
    , Generic
    )

instance (Binary t) => Binary (Label t)

{-# INLINE setLabelName #-}
setLabelName :: Name -> Label t -> Label t
setLabelName newName Label{labelTag} =
  Label
    { labelName = newName
    , ..
    }
