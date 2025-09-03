{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition.Alias (AliasDef (..)) where

import Coal.Language.Type (Parameter, ParameterizedType)
import Data.Data (Data, Typeable)

data AliasDef = AliasDef [Parameter ()] ParameterizedType
  deriving (Show, Eq, Ord, Read, Data, Typeable)
