{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition.Fold (FoldDefinition (..)) where

import Coal.Language.Expression (Clause (..))
import Coal.Language.Trait (Qualified (..))
import Coal.Language.Type (ParameterizedType)
import Data.Data (Data, Typeable)
import Data.List.NonEmpty (NonEmpty)

data FoldDefinition a t = FoldDefinition
  { foldDefinitionType :: Maybe (Qualified ParameterizedType)
  , foldDefinitionClauses :: NonEmpty (Clause a () t)
  }
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)
