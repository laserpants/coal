{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Pattern (Pattern (..), BoundNames (..)) where

import Noll.Label (Label (..))
import Noll.Language.Primitive (Primitive (..))
import Noll.Language.Type (Type, TypeId (..))
import Noll.Utils (Dictionary, Map, Name)

data Pattern a t
  = -- | Type-annotated pattern
    PAnnotation (Type TypeId ()) (Pattern a t)
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

class BoundNames p t where
  boundNames :: p -> [(Name, t)]

instance (BoundNames p t) => BoundNames [p] t where
  boundNames = concatMap boundNames

instance (BoundNames p t) => BoundNames (Maybe p) t where
  boundNames = concatMap boundNames

instance (BoundNames p t) => BoundNames (Map a p) t where
  boundNames = concatMap boundNames

instance BoundNames (Pattern a t) t where
  boundNames =
    \case
      PVariable _ (Label t name) ->
        [(name, t)]
      PAnnotation _ p ->
        boundNames p
      PConstructor _ _ ps ->
        boundNames ps
      PListLiteral _ _ ps ->
        boundNames ps
      PRecord _ _ p1 p2 ->
        boundNames p1 <> boundNames p2
      PListCons _ _ p1 p2 ->
        boundNames p1 <> boundNames p2
      PAny{} ->
        []
      PLiteral{} ->
        []
