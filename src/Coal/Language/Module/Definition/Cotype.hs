{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition.Cotype (CotypeDefinition (..)) where

import Coal.Language.Codata.Accessor (CodataAccessor (..))
import Coal.Language.Type (Parameter, ParameterizedType)
import Data.Data (Data, Typeable)

data CotypeDefinition = CotypeDefinition
  { cotypeDefinitionParameters :: [Parameter ()]
  , cotypeDefinitionAccessors :: [CodataAccessor Parameter () ParameterizedType]
  }
  deriving (Show, Eq, Ord, Read, Data, Typeable)
