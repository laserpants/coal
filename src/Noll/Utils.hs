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
  getAndModify,
  groupByEq,
  const2,
  traverseM,
  isConstructor,
  applyM1,
  applyM2,
  Over,
  (<$$>),
  (<$$$>),
  (&&.),
  (||.),
)
where

import Control.Monad (forM, forM_, mapM)
import Control.Monad.State (MonadState, get, modify)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Char (isAlpha, isUpper, ord)
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

type Over o n = (n -> n) -> o -> o

{-# INLINE (<$$>) #-}
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

infixr 8 <$$>

{-# INLINE (<$$$>) #-}
(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(<$$$>) = fmap . fmap . fmap

infixr 8 <$$$>

{-# INLINE (&&.) #-}
(&&.) :: (t -> Bool) -> (t -> Bool) -> t -> Bool
f &&. g = h where h e = f e && g e

infixr 3 &&.

{-# INLINE (||.) #-}
(||.) :: (t -> Bool) -> (t -> Bool) -> t -> Bool
f ||. g = h where h e = f e || g e

infixr 2 ||.

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

{-# INLINE traverseM #-}
traverseM :: (Monad m, Applicative f, Traversable t) => (a -> m (f a)) -> t a -> m (f (t a))
traverseM = sequenceA <$$$> traverse

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

{-# INLINE dropWhileNot #-}
dropWhileNot :: (Char -> Bool) -> Text -> Text
dropWhileNot = Text.dropWhile . fmap not

isConstructor :: Name -> Bool
isConstructor name
  | Text.null name = error "Empty name"
  | Text.null s = False
  | otherwise = isUpper (Text.head s)
 where
  s = dropWhileNot (isAlpha ||. ('_' ==)) name

applyM1 :: (Monad m) => (a -> m b) -> m a -> m b
applyM1 f a = do
  a1 <- a
  f a1

applyM2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
applyM2 f a b = do
  a1 <- a
  b1 <- b
  f a1 b1

applyM3 :: (Monad m) => (a -> b -> c -> m d) -> m a -> m b -> m c -> m d
applyM3 f a b c = do
  a1 <- a
  b1 <- b
  c1 <- c
  f a1 b1 c1
