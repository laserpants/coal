{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition.Cotype (CotypeDef (..)) where

import Coal.Language.Type (Parameter, ParameterizedType)
import Data.Data (Data, Typeable)
import Extras (Name)

data CotypeDef = CotypeDef [Parameter ()] [(Name, ParameterizedType)]
  deriving (Show, Eq, Ord, Read, Data, Typeable)
