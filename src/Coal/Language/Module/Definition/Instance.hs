{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition.Instance (InstanceDefinition (..)) where

import Coal.Language.Trait (Trait (..))
import Coal.Language.Type (ParameterizedType)
import Data.Data (Data, Typeable)

data InstanceDefinition d a k t = InstanceDefinition
  { instanceDefinitionParameters :: [Trait ParameterizedType]
  , instanceDefinitionType :: ParameterizedType
  , instanceDefinitionEntries :: [d a k t]
  }
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)
