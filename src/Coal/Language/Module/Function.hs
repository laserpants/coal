{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Function (FunctionDef (..)) where

import Coal.Language.Pattern (Pattern)
import Coal.Language.Trait (With (..))
import Coal.Language.Type (ParameterizedType)
import Data.Data (Data, Typeable)
import Data.List.NonEmpty (NonEmpty)

data FunctionDef e a t = FunctionDef a (Maybe (With ParameterizedType)) (With t) (NonEmpty (Pattern a t)) (e a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)
