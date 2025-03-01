{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.AST.HasFree (HasBound (..), HasFree (..), exceptNames) where

import Data.Data (Data)
import Data.Generics.Uniplate.Data (universeBi)
import Data.Set (Set, singleton)
import Noll.Common.List1 (NonEmpty)
import Noll.Label (Label (..), labelName)
import Noll.Language.Pattern (Pattern (..))
import Noll.Utils (Dictionary, Map, Name)

import qualified Data.Set as Set

class HasBound b where
  boundIn :: b -> Set Name

instance (HasBound b) => HasBound (Dictionary b) where
  boundIn = Set.unions . fmap boundIn

instance (HasBound b) => HasBound (Maybe b) where
  boundIn = Set.unions . fmap boundIn

instance (HasBound b) => HasBound [b] where
  boundIn = Set.unions . fmap boundIn

instance (HasBound b) => HasBound (NonEmpty b) where
  boundIn = Set.unions . fmap boundIn

instance HasBound (Label t) where
  boundIn (Label _ name) = singleton name

instance (Data a, Data t) => HasBound (Pattern a t) where
  boundIn = Set.fromList . universeBi

class HasFree f t where
  freeIn :: f -> Set (Label t)

instance (HasFree (Label t) t) where
  freeIn = Set.singleton

instance (Ord t, HasFree f t) => HasFree (Maybe f) t where
  freeIn = Set.unions . fmap freeIn

instance (Ord t, HasFree f t) => HasFree [f] t where
  freeIn = Set.unions . fmap freeIn

instance (Ord t, HasFree f t) => HasFree (NonEmpty f) t where
  freeIn = Set.unions . fmap freeIn

instance (Ord t, HasFree f t) => HasFree (Map a f) t where
  freeIn = Set.unions . fmap freeIn

instance (Ord t, HasFree s t) => HasFree (Set s) t where
  freeIn = Set.unions . Set.map freeIn

{-# INLINE exceptNames #-}
exceptNames :: (Foldable f) => Set (Label a) -> f Name -> Set (Label a)
exceptNames free bound = Set.filter (`notInNames` bound) free

{-# INLINE notInNames #-}
notInNames :: (Foldable f) => Label a -> f Name -> Bool
notInNames = notElem . labelName
