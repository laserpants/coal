{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}

module Noll.Utils (
  module Control.Monad,
  module Data.Foldable,
  module Data.Set,
  module Data.Map.Strict,
  Name,
  Dictionary,
  IndexMap,
  concatMapM,
  concatForM,
  unionMap,
  tellLeft,
  tellRight,
  fromMaybe,
  lexOrderRank,
  groupByEq,
  const2,
  (<$$>),
  (<$$$>),
) where

import Control.Monad (forM, forM_, mapM)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Char (ord)
import Data.Foldable (foldrM, traverse_)
import Data.Function (on)
import Data.List (groupBy)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set, unions)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

type Name = Text

type Dictionary = Map Name

type IndexMap = Map Int

{-# INLINE (<$$>) #-}
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

infixr 8 <$$>

{-# INLINE (<$$$>) #-}
(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(<$$$>) = fmap . fmap . fmap

infixr 8 <$$$>

{-# INLINE groupByEq #-}
groupByEq :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupByEq = groupBy . on (==)

-- | Monadic version of concatMap
{-# INLINE concatMapM #-}
concatMapM :: (Monad m, Traversable f) => (a -> m [b]) -> f a -> m [b]
concatMapM f xs = fmap concat (mapM f xs)

{-# INLINE concatForM #-}
concatForM :: (Monad m, Traversable f) => f a -> (a -> m [b]) -> m [b]
concatForM = flip concatMapM

{-# INLINE unionMap #-}
unionMap :: (Ord b) => (a -> Set b) -> Set a -> Set b
unionMap f = unions . Set.map f

{-# INLINE tellLeft #-}
tellLeft :: (MonadWriter [Either e a] m) => [e] -> m ()
tellLeft = tell . fmap Left

{-# INLINE tellRight #-}
tellRight :: (MonadWriter [Either e a] m) => [a] -> m ()
tellRight = tell . fmap Right

{-# INLINE const2 #-}
const2 :: a -> b -> c -> a
const2 a _ _ = a

lexOrderRank :: Text -> Int
lexOrderRank txt = sum [36 ^ i | i <- [1 .. len - 1]] + Text.foldl' go 0 txt
 where
  len = Text.length txt
  go acc c = acc * 36 + (ord c - if c `elem` ['0' .. '9'] then 22 else ord 'a')
