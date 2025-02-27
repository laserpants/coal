{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}

module Noll.Utils (
  module Control.Monad,
  module Data.Foldable,
  module Data.Set,
  module Data.Map.Strict,
  module Noll.Utils.Operators,
  module Noll.Utils.Name,
  module Noll.Utils.Data.Text,
  module Noll.Utils.Data.Functor,
  module Noll.Utils.Control.Monad,
  IndexMap,
  concatMapM,
  concatForM,
  unionMap,
  tellLeft,
  tellRight,
  fromMaybe,
  lexOrderRank,
  getAndModify,
  groupByEq,
  const2,
  traverseM,
  applyM1,
  applyM2,
  applyM3,
  Over,
)
where

import Control.Monad (forM, forM_, mapM)
import Control.Monad.State (MonadState, get, modify)
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
import Noll.Utils.Control.Monad
import Noll.Utils.Data.Functor
import Noll.Utils.Data.Text
import Noll.Utils.Name
import Noll.Utils.Operators

type IndexMap = Map Int

type Over o n = (n -> n) -> o -> o

{-# INLINE groupByEq #-}
groupByEq :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupByEq = groupBy . on (==)

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

getAndModify :: (MonadState s m) => (s -> s) -> m s
getAndModify f = do
  s <- get
  modify f
  return s

lexOrderRank :: Text -> Int
lexOrderRank text
  | Text.null text =
      error "Empty string"
  | otherwise =
      snd (Text.foldr f (0, 0) text) - 1
 where
  f :: Char -> (Int, Int) -> (Int, Int)
  f c (m, n) = (m + 1, n + (36 ^ m) + g (ord c))
  g n
    | n > 122 || n < 48 =
        error "Invalid character"
    | n >= 97 =
        n - 97
    | otherwise =
        n - 22
