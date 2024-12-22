{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Pattern (Pattern (..)) where

import Noll.Label (Label (..))
import Noll.Language.Primitive (Primitive (..))
import Noll.Utils (Dictionary)

data Pattern t
  = -- | Wildcard pattern
    Any t
  | -- | Variable pattern
    Variable (Label t)
  | -- | Data constructor pattern
    Constructor (Label t) [Pattern t]
  | -- | Literal pattern
    Literal Primitive
  | -- | Record pattern
    Record t (Dictionary (Pattern t)) (Maybe (Pattern t))
  | -- | List cons-operator
    ListCons t (Pattern t) (Pattern t)
  | -- | List literal
    ListLiteral t [Pattern t]
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
