{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Constant (Constant (..)) where

import Data.Data (Data, Typeable)

import Coal.Language.Trait (With (..))

data Constant e a t = Constant a (With t) (e a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)
