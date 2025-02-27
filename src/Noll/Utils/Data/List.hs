module Noll.Utils.Data.List (groupByEq) where

import Data.Function (on)
import Data.List (groupBy)

{-# INLINE groupByEq #-}
groupByEq :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupByEq = groupBy . on (==)
