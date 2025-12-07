{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition.Unfold (UnfoldDefinition (..)) where

import Coal.Language.Expression (Expression (..))
import Coal.Language.Pattern (Pattern (..))
import Coal.Language.Trait (With (..))
import Coal.Language.Type (ParameterizedType)
import Data.Data (Data, Typeable)
import Data.List.NonEmpty (NonEmpty)
import Extras (Dictionary)

data UnfoldDefinition a t = UnfoldDefinition
  { unfoldDefinitionType :: With ParameterizedType
  , unfoldDefinitionPatterns :: NonEmpty (Pattern a t)
  , unfoldDefinitionFields :: Dictionary (Expression a t)
  , unfoldDefinitionExpression :: Maybe (Expression a t)
  }
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)
