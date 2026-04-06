-- +
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Trait (Trait (..), With (..)) where

import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import Extras (Name)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..))

-- | Standalone type trait
data Trait t = Trait
  { traitName :: Name
  , traitType :: t
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

instance (Binary t) => Binary (Trait t)

instance (Pretty t) => Pretty (Trait t) where
  pretty (Trait name t) =
    pretty name <> "<" <> pretty t <> ">"

-- | Qualified type
data With t = With [Trait t] t
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

instance (Binary t) => Binary (With t)
