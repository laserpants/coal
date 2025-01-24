{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Common.Supply (
  Supply (..),
  supply,
  --  supplyN,
  supplied,
  suppliedName,
) where

import Control.Monad (replicateM)
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.State (MonadState, get, modify)
import Noll.Utils (Name)
import TextShow (showt)

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

-- supplyN :: (MonadState s m, Supply s) => Int -> m [s]
-- supplyN n = replicateM n supply

supplied :: (MonadState s m, Supply s) => (Int -> a) -> m a
supplied f = f . getSupply <$> supply

suppliedName :: (MonadReader Name m, MonadState s m, Supply s) => m Name
suppliedName = ask >>= supplied . freshName

{-# INLINE freshName #-}
freshName :: Name -> Int -> Name
freshName prefix index = "$" <> prefix <> "." <> showt index
