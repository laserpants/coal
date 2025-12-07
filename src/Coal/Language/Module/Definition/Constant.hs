{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition.Constant (ConstantDef (..)) where

import Coal.Language.Expression (Expression)
import Coal.Language.Trait (With (..))
import Coal.Language.Type (ParameterizedType)
import Data.Data (Data, Typeable)

data ConstantDef a t = ConstantDef
  { constantDefMetadata :: a
  , constantDefAnnotation :: Maybe (With ParameterizedType)
  , constantDefType :: With t
  , constantDefExpression :: Expression a t
  }
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)
