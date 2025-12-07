{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition.Trait (TraitDef (..)) where

import Coal.Language.Trait (Trait (..))
import Coal.Language.Type (Parameter, Type (..))
import Coal.Language.Type.Scheme (Scheme (..))
import Data.Data (Data, Typeable)
import Extras (Name)

data TraitDef k = TraitDef
  { traitDefRequiredTraits :: [Trait (Parameter k)]
  , traitDefParameter :: Parameter k
  , traitDefMethods :: [(Name, Scheme Parameter k (Type Parameter k))]
  }
  deriving (Show, Eq, Ord, Read, Data, Typeable)
