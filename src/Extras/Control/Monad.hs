module Extras.Control.Monad (
  concatMapM,
  concatForM,
  traverseM,
  applyM1,
  applyM2,
  applyM3,
) where

import Extras.Data.Functor ((<$$$>))

-- | Monadic version of concatMap
{-# INLINE concatMapM #-}
concatMapM :: (Monad m, Traversable f) => (a -> m [b]) -> f a -> m [b]
concatMapM f xs = concat <$> mapM f xs

-- | concatMapM with the arguments flipped
{-# INLINE concatForM #-}
concatForM :: (Monad m, Traversable f) => f a -> (a -> m [b]) -> m [b]
concatForM = flip concatMapM

{-# INLINE traverseM #-}
traverseM :: (Monad m, Applicative f, Traversable t) => (a -> m (f a)) -> t a -> m (f (t a))
traverseM = sequenceA <$$$> traverse

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
