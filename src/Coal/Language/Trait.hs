{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Trait (Trait (..), With (..), traitName) where

import Data.Data (Data, Typeable)
import Extra (Name)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..), parens)

-- | Standalone type trait
data Trait t = Trait Name t
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable, Generic)

instance (Pretty t) => Pretty (Trait t) where
  pretty (Trait name t) =
    pretty name <> parens (pretty t)

data With t = With [Trait t] t
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)

{-# INLINE traitName #-}
traitName :: Trait t -> Name
traitName (Trait name _) = name
