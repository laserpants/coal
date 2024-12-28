{-# LANGUAGE StrictData #-}

module Noll.Utils (
  module Control.Monad,
  module Data.Foldable,
  module Data.Set,
  module Data.Map.Strict,
  module Data.List.NonEmpty,
  Name,
  Dictionary,
  IndexMap,
  Some,
  concatMapM,
  unionMap,
  (<$$>),
  (<$$$>),
) where

import Control.Monad (forM, forM_, liftM)
import Data.Foldable (foldrM, traverse_)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set, unions)
import qualified Data.Set as Set
import Data.Text (Text)

type Name = Text

type Dictionary = Map Name

type IndexMap = Map Int

type Some = NonEmpty

{-# INLINE (<$$>) #-}
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

infixr 8 <$$>

{-# INLINE (<$$$>) #-}
(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(<$$$>) = fmap . fmap . fmap

infixr 8 <$$$>

-- | Monadic version of concatMap
concatMapM :: (Monad m, Traversable f) => (a -> m [b]) -> f a -> m [b]
concatMapM f xs = liftM concat (mapM f xs)

{-# INLINE unionMap #-}
unionMap :: (Ord b) => (a -> Set b) -> Set a -> Set b
unionMap f = unions . Set.map f
