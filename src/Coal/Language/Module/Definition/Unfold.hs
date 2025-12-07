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
import Extras (Dictionary)

data UnfoldDef a t = UnfoldDef
  { unfoldDefType :: With ParameterizedType
  , unfoldDefPatterns :: NonEmpty (Pattern a t)
  , unfoldDefFields :: Dictionary (Expression a t)
  , unfoldDefExpression :: Maybe (Expression a t)
  }
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)
