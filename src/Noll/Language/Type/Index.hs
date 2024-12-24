{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Noll.Language.Type.Index (TypeIndex (..), HasActive (..), activeIdsIn) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as Set

data TypeIndex k = TypeIndex
  { indexKind :: k
  , indexId :: Int
  }
  deriving (Show, Eq, Ord, Read, Functor, Foldable)

class HasActive k t | t -> k where
  activeIn :: t -> Set (TypeIndex k)

instance (Ord k, HasActive k t) => HasActive k (Map a t) where
  activeIn = Set.unions . fmap activeIn

instance (Ord k, HasActive k t) => HasActive k [t] where
  activeIn = Set.unions . fmap activeIn

instance (Ord k, HasActive k t) => HasActive k (NonEmpty t) where
  activeIn = Set.unions . fmap activeIn

activeIdsIn :: (HasActive k t) => t -> Set Int
activeIdsIn t = Set.map indexId (activeIn t)
