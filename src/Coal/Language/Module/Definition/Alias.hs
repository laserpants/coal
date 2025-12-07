{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition.Alias (AliasDefinition (..)) where

import Coal.Language.Type (Parameter, ParameterizedType)
import Data.Data (Data, Typeable)

data AliasDefinition = AliasDefinition
  { aliasDefinitionParameters :: [Parameter ()]
  , aliasDefinitionType :: ParameterizedType
  }
  deriving (Show, Eq, Ord, Read, Data, Typeable)
