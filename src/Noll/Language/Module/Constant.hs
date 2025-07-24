{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Module.Constant (Constant (..)) where

import Data.Data (Data, Typeable)

import Noll.Language.Trait (With (..))

data Constant e a t = Constant a (With t) (e a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)
