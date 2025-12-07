{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition.Constant (ConstantDefinition (..)) where

import Coal.Language.Expression (Expression)
import Coal.Language.Trait (With (..))
import Coal.Language.Type (ParameterizedType)
import Data.Data (Data, Typeable)

data ConstantDefinition a t = ConstantDefinition
  { constantDefinitionMetadata :: a
  , constantDefinitionAnnotation :: Maybe (With ParameterizedType)
  , constantDefinitionType :: With t
  , constantDefinitionExpression :: Expression a t
  }
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)
