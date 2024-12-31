{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Pattern (Pattern (..)) where

import Noll.Label (Label (..))
import Noll.Language.Primitive (Primitive (..))
import Noll.Language.Type (Type, TypeVariable (..))
import Noll.Utils (Dictionary, Map, Name)

data Pattern a t
  = -- | Type-annotated pattern
    PAnnotation (Type TypeVariable ()) (Pattern a t)
  | -- | Wildcard pattern
    PAny a t
  | -- | Variable pattern
    PVariable a (Label t)
  | -- | Data constructor pattern
    PConstructor a (Label t) [Pattern a t]
  | -- | Literal pattern
    PLiteral a Primitive
  | -- | Record pattern
    PRecord a t (Dictionary (Pattern a t)) (Maybe (Pattern a t))
  | -- | List cons-operator
    PListCons a t (Pattern a t) (Pattern a t)
  | -- | List literal
    PListLiteral a t [Pattern a t]
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
