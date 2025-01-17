{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Common.Supply (Supply (..), supply, supplyN, supplied) where

import Control.Monad (replicateM)
import Control.Monad.State (MonadState, get, modify)

class Supply s where
  updateSupply :: (Int -> Int) -> s -> s
  getSupply :: s -> Int

instance Supply Int where
  updateSupply = id
  getSupply = id

supply :: (MonadState s m, Supply s) => m s
supply = do
  n <- get
  modify (updateSupply succ)
  pure n

supplyN :: (MonadState s m, Supply s) => Int -> m [s]
supplyN n = replicateM n supply

supplied :: (MonadState s m, Supply s) => (Int -> a) -> m a
supplied f = f . getSupply <$> supply
