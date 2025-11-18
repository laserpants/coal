{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

module Coal.Language.Pattern (Pattern (..), IndexedPattern) where

import Coal.Common.FreeVars (BoundVars (..))
import Coal.Common.Label (Label (..))
import Coal.Language.Primitive (Primitive (..))
import Coal.Language.Trait (Trait (..))
import Coal.Language.Type (Parameter (..), Type, TypeIndex)
import Coal.Language.Type.Kind (Kind (..))
import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.NonEmpty (NonEmpty)
import Data.Set (unions)
import qualified Data.Set as Set
import Extras (Dictionary, Name)

data Pattern a t
  = -- | Type-annotated pattern
    PAnnotation a (Type Parameter ()) (Pattern a t)
  | -- | Wildcard pattern
    PAny a t
  | -- | Variable pattern
    PVariable a (Label t)
  | -- | Data constructor pattern
    PConstructor a (Label t) [Pattern a t]
  | -- | Integer literal pattern
    PInteger a t Integer
  | -- | Literal pattern
    PLiteral a Primitive
  | -- | Record pattern
    PRecord a t (Dictionary (Pattern a t)) (Maybe (Pattern a t))
  | -- | List cons-operator
    PListCons a t (Pattern a t) (Pattern a t)
  | -- | List literal
    PListLiteral a t [Pattern a t]
  | -- | Tuple pattern
    PTuple a t (NonEmpty (Pattern a t))
  | -- | Or-pattern
    POr a t (Pattern a t) (Pattern a t)
  | -- | As-pattern
    PAs a (Label t) (Pattern a t)
  | -- | Shorthand variable binding of the form { name }, which desugars to { name = name }
    PShorthand a (Label t)
  | -- | Recursive operator pattern used in fold expressions
    PAtVariable a (Label t)
  | -- | Pattern fold
    PNamedFold a Name (Label t)
  | -- | Trait instance dictionary
    PTraitDictionary a t (Trait t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)

instance (Data a, Data t) => BoundVars (Pattern a t) where
  boundIn =
    \case
      PRecord _ _ d mp ->
        boundIn mp <> unions (fmap boundIn d)
      p ->
        Set.fromList (universeBi p)

type IndexedPattern a = Pattern a (Type TypeIndex Kind)
