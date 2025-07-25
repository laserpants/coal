{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Function (Function (..)) where

import Coal.Common.List1 (List1)
import Coal.Language.Pattern (Pattern)
import Coal.Language.Trait (With (..))
import Data.Data (Data, Typeable)

data Function e a t = Function a (With t) (List1 (Pattern a t)) (e a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)
