{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}

module Noll.Language.Pattern (Pattern (..)) where

import Data.Data (Data, Typeable)
import Noll.Common.List1 (List1)
import Noll.Label (Label (..))
import Noll.Language.Primitive (Primitive (..))
import Noll.Language.Type (Parameter (..), Type)
import Noll.Utils (Dictionary)

data Pattern a t
  = -- | Type-annotated pattern
    PAnnotation a (Type Parameter ()) (Pattern a t)
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
  | -- | Or-pattern
    PTuple a t (List1 (Pattern a t))
  | -- | Pattern matching expression
    POr a t (Pattern a t) (Pattern a t)
  | -- | TODO
    PShorthand a (Label t)
  | -- | TODO
    PAtVariable a (Label t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)
