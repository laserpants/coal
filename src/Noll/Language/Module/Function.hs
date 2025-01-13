{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Module.Function (Function (..)) where

import Noll.Common.List1 (List1, NonEmpty ((:|)))
import Noll.Label (Label (..))
import Noll.Language.Expression (Expression (..))
import Noll.Language.Expression.Binding (Binding (..))
import Noll.Language.HasType (HasType (..))
import Noll.Language.Pattern (Pattern)
import Noll.Language.Trait (Uses (..))
import Noll.Language.Type (IndexedType, foldType)
import Noll.Utils (Name)

data Function e a t = Function a (Uses t) (List1 (Pattern a t)) (e a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
