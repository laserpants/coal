{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Library.Supply (Store (..), supply, supplyN, update, load, save, over) where

import Control.Monad.State (MonadState, gets, modify)
import Control.Monad (replicateM)

class Store v s where
  obtain :: s -> v
  inside :: (v -> v) -> s -> s

instance Store s s where
  obtain = id
  inside = id

{-# INLINE update #-}
update :: (Store v s) => v -> s -> s
update = inside . const

{-# INLINE load #-}
load :: (MonadState s m, Store a s) => m a
load = gets obtain

{-# INLINE save #-}
save :: (MonadState s m, Store a s) => a -> m ()
save = modify . update

{-# INLINE over #-}
over :: (MonadState s m, Store a s) => (a -> a) -> m ()
over = modify . inside

supply :: (Num n, MonadState s m, Store n s) => m n
supply = do
  n <- load
  save (n + 1)
  return n

supplyN :: (Num n, MonadState s m, Store n s) => Int -> m [n]
supplyN n = replicateM n supply
