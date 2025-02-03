{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Module.TraitInstance (TraitInstance (..)) where

import Noll.Language.Module.Constant (Constant)
import Noll.Language.Module.Function (Function)

data TraitInstance e a t
  = TFunction (Function e a t)
  | TConstant (Constant e a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
