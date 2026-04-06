{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition.Constant (ConstantDefinition (..)) where

import Coal.Language.Expression (Expression)
import Coal.Language.Trait (Qualified (..))
import Coal.Language.Type (ParameterizedType)
import Data.Data (Data, Typeable)

data ConstantDefinition a t = ConstantDefinition
  { constantDefinitionMetadata :: a
  , constantDefinitionAnnotation :: Maybe (Qualified ParameterizedType)
  , constantDefinitionType :: Qualified t
  , constantDefinitionExpression :: Expression a () t
  }
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)
