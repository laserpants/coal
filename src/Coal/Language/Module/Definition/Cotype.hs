{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition.Cotype (CotypeDef (..)) where

import Coal.Language.CodataAccessor (CodataAccessor (..))
import Coal.Language.Type (Parameter, ParameterizedType)
import Data.Data (Data, Typeable)

data CotypeDef = CotypeDef [Parameter ()] [CodataAccessor Parameter () ParameterizedType]
  deriving (Show, Eq, Ord, Read, Data, Typeable)
