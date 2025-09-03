{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Cotype (Cotype (..)) where

import Coal.Language.Type (Parameter, ParameterizedType)
import Data.Data (Data, Typeable)
import Extra (Name)

data Cotype = Cotype [Parameter ()] [(Name, ParameterizedType)]
  deriving (Show, Eq, Ord, Read, Data, Typeable)
