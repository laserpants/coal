module Extras.Control.Applicative (pure1, pure2, pure3, twice) where

import Control.Monad (replicateM_)
import Extras.Data.Functor ((<$$>))

{-# INLINE pure1 #-}
pure1 :: (Applicative f) => (a -> b) -> a -> f b
pure1 f = pure . f

{-# INLINE pure2 #-}
pure2 :: (Applicative f1, Functor f2) => (a -> b) -> f2 a -> f1 (f2 b)
pure2 f = pure . (f <$>)

{-# INLINE pure3 #-}
pure3 :: (Applicative f1, Functor f2, Functor f3) => (a -> b) -> f2 (f3 a) -> f1 (f2 (f3 b))
pure3 f = pure . (f <$$>)

{-# INLINE twice #-}
twice :: (Applicative m) => m a -> m ()
twice = replicateM_ 2
