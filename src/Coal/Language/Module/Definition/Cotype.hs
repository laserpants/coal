{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition.Cotype (CotypeDef (..)) where

import Coal.Language.Codata.Accessor (CodataAccessor (..))
import Coal.Language.Type (Parameter, ParameterizedType)
import Data.Data (Data, Typeable)

data CotypeDef = CotypeDef
  { cotypeDefParameters :: [Parameter ()]
  , cotypeDefAccessors :: [CodataAccessor Parameter () ParameterizedType]
  }
  deriving (Show, Eq, Ord, Read, Data, Typeable)
