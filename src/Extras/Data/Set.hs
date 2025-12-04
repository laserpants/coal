module Extras.Data.Set (unionMap) where

import Data.Set (Set, unions)

{-# INLINE unionMap #-}
unionMap :: (Foldable f, Functor f, Ord b) => (a -> Set b) -> f a -> Set b
unionMap f = unions . fmap f
