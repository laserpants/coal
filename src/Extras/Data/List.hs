module Extras.Data.List (groupByEq, groupByKey) where

import Data.Function (on)
import Data.List (groupBy)
import qualified Data.Map as Map

{-# INLINE groupByEq #-}
groupByEq :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupByEq = groupBy . on (==)

groupByKey :: (Ord a) => [(a, b)] -> [(a, [b])]
groupByKey = Map.toList . Map.fromListWith (++) . map (\(k, v) -> (k, [v]))
