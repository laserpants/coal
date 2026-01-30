{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition.Function (FunctionDefinition (..)) where

import Coal.Language.Expression (Expression)
import Coal.Language.Pattern (Pattern)
import Coal.Language.Trait (With (..))
import Coal.Language.Type (ParameterizedType)
import Data.Data (Data, Typeable)
import Data.List.NonEmpty (NonEmpty)

data FunctionDefinition a t = FunctionDefinition
  { functionDefinitionMetadata :: a
  , functionDefinitionAnnotation :: Maybe (With ParameterizedType)
  , functionDefinitionType :: With t
  , functionDefinitionPatterns :: NonEmpty (Pattern a () t)
  , functionDefinitionExpression :: Expression a () t
  }
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)
