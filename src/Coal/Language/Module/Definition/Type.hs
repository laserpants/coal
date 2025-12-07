{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition.Type (TypeDef (..)) where

import Coal.Language.Data.Constructor (DataConstructor (..))
import Coal.Language.Type (Parameter, ParameterizedType)
import Data.Data (Data, Typeable)

data TypeDef = TypeDef
  { typeDefParameters :: [Parameter ()]
  , typeDefConstructors :: [DataConstructor Parameter () ParameterizedType]
  }
  deriving (Show, Eq, Ord, Read, Data, Typeable)
