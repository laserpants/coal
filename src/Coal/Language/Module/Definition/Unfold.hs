{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition.Unfold (UnfoldDef (..)) where

import Coal.Language.Expression (Expression (..))
import Coal.Language.Pattern (Pattern (..))
import Coal.Language.Trait (With (..))
import Coal.Language.Type (ParameterizedType)
import Data.Data (Data, Typeable)
import Data.List.NonEmpty (NonEmpty)
import Extra (Dictionary)

data UnfoldDef a t = UnfoldDef (With ParameterizedType) (NonEmpty (Pattern a t)) (Dictionary (Expression a t)) (Maybe (Expression a t))
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)
