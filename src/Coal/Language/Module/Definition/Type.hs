{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition.Type (TypeDefinition (..)) where

import Coal.Language.Data.Constructor (DataConstructor (..))
import Coal.Language.Type (Parameter, ParameterizedType)
import Data.Data (Data, Typeable)

data TypeDefinition = TypeDefinition
  { typeDefinitionParameters :: [Parameter ()]
  , typeDefinitionConstructors :: [DataConstructor Parameter () ParameterizedType]
  }
  deriving (Show, Eq, Ord, Read, Data, Typeable)
