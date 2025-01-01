{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Library.Supply (Supply (..), supply, supplyN) where

import Control.Monad (replicateM)
import Control.Monad.State (MonadState, get, modify)

class Supply s where
  updateSupply :: (Int -> Int) -> s -> s

instance Supply Int where
  updateSupply = id

supply :: (MonadState s m, Supply s) => m s
supply = do
  n <- get
  modify (updateSupply (+ 1))
  pure n

supplyN :: (MonadState s m, Supply s) => Int -> m [s]
supplyN n = replicateM n supply
