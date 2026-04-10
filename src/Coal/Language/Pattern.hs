-- +
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Pattern (Pattern (..)) where

import Coal.Common.FreeVars (BoundVars (..))
import Coal.Common.Label (Label (..))
import Coal.Common.Name (Dictionary, Name)
import Coal.Language.Primitive (Primitive (..))
import Coal.Language.Trait (Trait (..))
import Coal.Language.Type (Parameter (..), Type)
import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set
import GHC.Generics (Generic)

data Pattern a s t
  = -- | Type-annotated pattern
    PAnnotation a (Type Parameter s) (Pattern a s t)
  | -- | Wildcard pattern
    PAny a t
  | -- | Variable pattern
    PVariable a (Label t)
  | -- | Data constructor pattern
    PConstructor a (Label t) [Pattern a s t]
  | -- | Integer literal pattern
    PInteger a t Integer
  | -- | Literal pattern
    PLiteral a Primitive
  | -- | Record pattern
    PRecord a t (Dictionary (Pattern a s t)) (Maybe (Pattern a s t))
  | -- | List cons-operator
    PListCons a t (Pattern a s t) (Pattern a s t)
  | -- | List literal
    PListLiteral a t [Pattern a s t]
  | -- | Tuple pattern
    PTuple a t (NonEmpty (Pattern a s t))
  | -- | Or-pattern
    POr a t (Pattern a s t) (Pattern a s t)
  | -- | As-pattern
    PAs a (Label t) (Pattern a s t)
  | -- | Shorthand variable binding of the form { name }, which desugars to { name = name }
    PShorthand a (Label t)
  | -- | Recursive operator pattern used in fold expressions
    PAtVariable a (Label t)
  | -- | Pattern fold
    PNamedFold a Name (Label t)
  | -- | Trait instance dictionary
    PTraitInstance a t (Trait t)
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Functor
    , Foldable
    , Traversable
    , Data
    , Typeable
    , Generic
    )

instance (Binary a, Binary s, Binary t) => Binary (Pattern a s t)

instance (Data a, Data s, Data t) => BoundVars (Pattern a s t) where
  boundIn =
    \case
      PRecord _ _ d p ->
        boundIn d <> boundIn p
      PConstructor _ _ ps ->
        boundIn ps
      PAnnotation _ _ p ->
        boundIn p
      PAs _ ll p ->
        boundIn ll <> boundIn p
      p ->
        Set.fromList (universeBi p)
