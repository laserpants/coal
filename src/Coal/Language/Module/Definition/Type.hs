{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition.Type (TypeDef (..)) where

import Coal.Language.DataConstructor (DataConstructor (..))
import Coal.Language.Type (Parameter, ParameterizedType)
import Data.Data (Data, Typeable)

data TypeDef = TypeDef [Parameter ()] [DataConstructor Parameter () ParameterizedType]
  deriving (Show, Eq, Ord, Read, Data, Typeable)
