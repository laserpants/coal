{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition.Trait (TraitDef (..)) where

import Coal.Language.Trait (Trait (..))
import Coal.Language.Type (Parameter, Type (..))
import Coal.Language.Type.Scheme (Scheme (..))
import Data.Data (Data, Typeable)
import Extras (Name)

data TraitDef k = TraitDef [Trait (Parameter k)] (Parameter k) [(Name, Scheme Parameter k (Type Parameter k))]
  deriving (Show, Eq, Ord, Read, Data, Typeable)
