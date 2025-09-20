{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Common.Supply (Supply (..), supply, supplied, freshName) where

import Control.Monad.State (MonadState)
import Extra (Name, getAndModify)
import TextShow (showt)

class Supply s where
  updateSupply :: (Int -> Int) -> s -> s
  getSupply :: s -> Int

instance Supply Int where
  updateSupply = id
  getSupply = id

supply :: (MonadState s m, Supply s) => m s
supply = getAndModify (updateSupply succ)

{-# INLINE supplied #-}
supplied :: (MonadState s m, Supply s) => (Int -> a) -> m a
supplied f = f . getSupply <$> supply

{-# INLINE freshName #-}
freshName :: Name -> Int -> Name
freshName prefix index = "$" <> prefix <> "." <> showt index
