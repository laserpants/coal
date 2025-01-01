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
  unionMap,
  tellLeft,
  tellRight,
  fromMaybe,
  (<$$>),
  (<$$$>),
) where

import Control.Monad (forM, forM_, liftM)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Foldable (foldrM, traverse_)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set, unions)
import qualified Data.Set as Set
import Data.Text (Text)

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

-- | Monadic version of concatMap
concatMapM :: (Monad m, Traversable f) => (a -> m [b]) -> f a -> m [b]
concatMapM f xs = liftM concat (mapM f xs)

{-# INLINE unionMap #-}
unionMap :: (Ord b) => (a -> Set b) -> Set a -> Set b
unionMap f = unions . Set.map f

{-# INLINE tellLeft #-}
tellLeft :: (MonadWriter [Either e a] m) => [e] -> m ()
tellLeft = tell . fmap Left

{-# INLINE tellRight #-}
tellRight :: (MonadWriter [Either e a] m) => [a] -> m ()
tellRight = tell . fmap Right
