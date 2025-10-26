module Extras.Data.Traversable (traverse2, forSM, forSM_) where

import Control.Monad (forM, void)

{-# INLINE traverse2 #-}
traverse2 :: (Applicative f, Traversable t1, Traversable t2) => (a -> f b) -> t2 (t1 a) -> f (t2 (t1 b))
traverse2 = traverse . traverse

{-# INLINE forSM #-}
forSM :: (Monad m, Enum n) => n -> [a] -> (a -> n -> m b) -> m [b]
forSM n vs = forM (zip vs [n ..]) . uncurry

{-# INLINE forSM_ #-}
forSM_ :: (Monad m, Enum n) => n -> [a] -> (a -> n -> m b) -> m ()
forSM_ n vs = void . forSM n vs
