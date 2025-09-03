{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Fold (FoldDef (..)) where

import Coal.Language.Expression (Clause (..), Expression (..))
import Coal.Language.Trait (With (..))
import Coal.Language.Type (ParameterizedType)
import Data.Data (Data, Typeable)
import Data.List.NonEmpty (NonEmpty)

data FoldDef a t = FoldDef (With ParameterizedType) (NonEmpty (Clause a t)) (Maybe (Expression a t))
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)
