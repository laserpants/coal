{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Library.Supply (Supply, supply, supplyN, update, load, save, over) where

import Control.Monad (replicateM)
import Control.Monad.State (MonadState, gets, modify)

class Supply v s where
  count :: s -> v
  overSupply :: (v -> v) -> s -> s

instance Supply s s where
  count = id
  overSupply = id

{-# INLINE update #-}
update :: (Supply v s) => v -> s -> s
update = overSupply . const

{-# INLINE load #-}
load :: (MonadState s m, Supply a s) => m a
load = gets count

{-# INLINE save #-}
save :: (MonadState s m, Supply a s) => a -> m ()
save = modify . update

{-# INLINE over #-}
over :: (MonadState s m, Supply a s) => (a -> a) -> m ()
over = modify . overSupply

supply :: (Num n, MonadState s m, Supply n s) => m n
supply = do
  n <- load
  save (n + 1)
  return n

supplyN :: (Num n, MonadState s m, Supply n s) => Int -> m [n]
supplyN n = replicateM n supply
