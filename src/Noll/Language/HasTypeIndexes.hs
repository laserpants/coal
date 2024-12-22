{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.HasTypeIndexes (HasTypeIndexes (..)) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Noll.Language.Trait (Trait (..))

class HasTypeIndexes o k t where
  typeIndexesIn :: t -> Set (o k)

instance (Ord k, Ord (o k), HasTypeIndexes o k t) => HasTypeIndexes o k (Map a t) where
  typeIndexesIn = mapTypeIndexesIn

instance (Ord k, Ord (o k), HasTypeIndexes o k t) => HasTypeIndexes o k (Maybe t) where
  typeIndexesIn = mapTypeIndexesIn

instance (Ord k, Ord (o k), HasTypeIndexes o k t) => HasTypeIndexes o k [t] where
  typeIndexesIn = mapTypeIndexesIn

instance (Ord k, Ord (o k), HasTypeIndexes o k t) => HasTypeIndexes o k (NonEmpty t) where
  typeIndexesIn = mapTypeIndexesIn

instance (Ord k, Ord (o k), HasTypeIndexes o k t) => HasTypeIndexes o k (Trait t) where
  typeIndexesIn = mapTypeIndexesIn

mapTypeIndexesIn :: (Functor f, Foldable f, HasTypeIndexes o k t, Ord (o k)) => f t -> Set (o k)
mapTypeIndexesIn = Set.unions . fmap typeIndexesIn
