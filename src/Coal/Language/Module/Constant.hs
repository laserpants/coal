{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Constant (ConstantDef (..)) where

import Coal.Language.Trait (With (..))
import Coal.Language.Type (ParameterizedType)
import Data.Data (Data, Typeable)

data ConstantDef e a t = ConstantDef a (Maybe (With ParameterizedType)) (With t) (e a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)
