{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Library.Supply (Supply (..), supply, supplyN) where

import Control.Monad (replicateM)
import Control.Monad.State (MonadState, get, gets, modify, put)

class Supply v s where
  updateSupply :: (v -> v) -> s -> s

instance Supply s s where
  updateSupply = id

supply :: (MonadState s m, Supply Int s) => m s
supply = do
  n <- get
  modify (updateSupply ((+ 1) :: Int -> Int))
  pure n

supplyN :: (MonadState s m, Supply Int s) => Int -> m [s]
supplyN n = replicateM n supply
