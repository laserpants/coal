{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition.Trait (TraitDefinition (..)) where

import Coal.Language.Trait (Trait (..))
import Coal.Language.Type (Parameter, Type (..))
import Coal.Language.Type.Scheme (Scheme (..))
import Data.Data (Data, Typeable)
import Extras (Name)

data TraitDefinition k = TraitDefinition
  { traitDefinitionRequired :: [Trait (Parameter k)]
  , traitDefinitionParameter :: Parameter k
  , traitDefinitionMethods :: [(Name, Scheme Parameter k (Type Parameter k))]
  }
  deriving (Show, Eq, Ord, Read, Data, Typeable)
