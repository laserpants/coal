{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.HasActive (HasActive (..)) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set, intersection, union)
import qualified Data.Set as Set
import Noll.Language.HasTypeIndexes (HasTypeIndexes (..))
import Noll.Language.Type.Index (TypeIndex (..))
import Noll.TypeSystem.Constraint (TypeConstraint (..))

class HasActive k t where
  activeIn :: t -> Set (TypeIndex k)

instance (Ord k, HasActive k t) => HasActive k (Map a t) where
  activeIn = Set.unions . fmap activeIn

instance (Ord k, HasActive k t) => HasActive k [t] where
  activeIn = Set.unions . fmap activeIn

instance (Ord k, HasActive k t) => HasActive k (NonEmpty t) where
  activeIn = Set.unions . fmap activeIn

instance (Ord k, HasTypeIndexes k t) => HasActive k (TypeConstraint TypeIndex k t) where
  activeIn =
    \case
      Equality t1 t2 ->
        typeIndexesIn t1 `union` typeIndexesIn t2
      Implicit t1 t2 m ->
        typeIndexesIn t1 `union` (typeIndexesIn t2 `intersection` typeIndexesIn m)
      Explicit t s ->
        typeIndexesIn t `union` typeIndexesIn s
