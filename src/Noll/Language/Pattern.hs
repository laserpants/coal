{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}

module Noll.Language.Pattern (Pattern (..)) where

import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (universeBi)
import Lang.Common.List1 (List1)
import Lang.FreeVars (BoundVars (..))
import Lang.Label (Label (..))
import Lang.Utils (Dictionary)
import Noll.Language.Primitive (Primitive (..))
import Noll.Language.Type (Parameter (..), Type)

import qualified Data.Set as Set

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
  | -- | Shorthand variable binding of the form { name }, which desugars to { name = name }
    PShorthand a (Label t)
  | -- | Recursion operator pattern used in fold catamorphisms
    PAtVariable a (Label t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)

instance (Data a, Data t) => BoundVars (Pattern a t) where
  boundIn = Set.fromList . universeBi
