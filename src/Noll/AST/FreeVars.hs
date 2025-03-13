{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.AST.FreeVars (BoundVars (..), FreeVars (..), exceptNames) where

import Data.Set (Set, singleton)
import Noll.Common.List1 (NonEmpty)
import Noll.Label (Label (..), labelName)
import Noll.Utils (Dictionary, Map, Name)

import qualified Data.Set as Set

class BoundVars b where
  boundIn :: b -> Set Name

instance (BoundVars b) => BoundVars (Dictionary b) where
  boundIn = Set.unions . fmap boundIn

instance (BoundVars b) => BoundVars (Maybe b) where
  boundIn = Set.unions . fmap boundIn

instance (BoundVars b) => BoundVars [b] where
  boundIn = Set.unions . fmap boundIn

instance (BoundVars b) => BoundVars (NonEmpty b) where
  boundIn = Set.unions . fmap boundIn

instance BoundVars (Label t) where
  boundIn (Label _ name) = singleton name

class FreeVars f t where
  freeIn :: f -> Set (Label t)

instance (FreeVars (Label t) t) where
  freeIn = Set.singleton

instance (Ord t, FreeVars f t) => FreeVars (Maybe f) t where
  freeIn = Set.unions . fmap freeIn

instance (Ord t, FreeVars f t) => FreeVars [f] t where
  freeIn = Set.unions . fmap freeIn

instance (Ord t, FreeVars f t) => FreeVars (NonEmpty f) t where
  freeIn = Set.unions . fmap freeIn

instance (Ord t, FreeVars f t) => FreeVars (Map a f) t where
  freeIn = Set.unions . fmap freeIn

instance (Ord t, FreeVars s t) => FreeVars (Set s) t where
  freeIn = Set.unions . Set.map freeIn

{-# INLINE exceptNames #-}
exceptNames :: (Foldable f) => Set (Label a) -> f Name -> Set (Label a)
exceptNames free bound = Set.filter (`notInNames` bound) free

{-# INLINE notInNames #-}
notInNames :: (Foldable f) => Label a -> f Name -> Bool
notInNames = notElem . labelName
