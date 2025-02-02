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
  Over,
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
import Data.Text (Text)

import qualified Data.Set as Set
import qualified Data.Text as Text

type Name = Text

type Dictionary = Map Name

type IndexMap = Map Int

type Over o n = (n -> n) -> o -> o

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

-- | concatMapM with the arguments flipped
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
lexOrderRank text
  | Text.null text =
      error "Empty string"
  | otherwise =
      snd (Text.foldr f (0, 0) text) - 1
 where
  f c (m, n) = (m + 1, n + (36 ^ m) + g (ord c))
  g n
    | n > 122 || n < 48 =
        error "Invalid character"
    | n >= 97 =
        n - 97
    | otherwise =
        n - 22
