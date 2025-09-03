{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Instance (InstanceDef (..)) where

import Coal.Language.Trait (Trait (..))
import Coal.Language.Type (ParameterizedType)
import Data.Data (Data, Typeable)

data InstanceDef d a k t = InstanceDef [Trait ParameterizedType] ParameterizedType [d a k t]
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)
