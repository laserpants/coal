{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Common.Supply (
  Supply (..),
  supply,
  supplied,
  suppliedName,
) where

import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.State (MonadState)
import Noll.Utils (Name, getAndModify)
import TextShow (showt)

class Supply s where
  updateSupply :: (Int -> Int) -> s -> s
  getSupply :: s -> Int

instance Supply Int where
  updateSupply = id
  getSupply = id

supply :: (MonadState s m, Supply s) => m s
supply = getAndModify (updateSupply succ)

suppliedName :: (MonadReader Name m, MonadState s m, Supply s) => m Name
suppliedName = ask >>= supplied . freshName

{-# INLINE supplied #-}
supplied :: (MonadState s m, Supply s) => (Int -> a) -> m a
supplied f = f . getSupply <$> supply

{-# INLINE freshName #-}
freshName :: Name -> Int -> Name
freshName prefix index = "$" <> prefix <> "." <> showt index
