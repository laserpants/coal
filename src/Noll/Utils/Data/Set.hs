module Noll.Utils.Data.Set (unionMap) where

import Data.Set (Set, unions)

import qualified Data.Set as Set

{-# INLINE unionMap #-}
unionMap :: (Ord b) => (a -> Set b) -> Set a -> Set b
unionMap f = unions . Set.map f
