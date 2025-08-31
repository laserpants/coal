{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Function (Function (..)) where

import Coal.Language.Pattern (Pattern)
import Coal.Language.Trait (With (..))
import Data.Data (Data, Typeable)
import Data.List.NonEmpty (NonEmpty)

data Function e a t = Function a (With t) (NonEmpty (Pattern a t)) (e a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)
