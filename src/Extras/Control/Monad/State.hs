module Extras.Control.Monad.State (getAndModify) where

import Control.Monad.State (MonadState, get, modify)

getAndModify :: (MonadState s m) => (s -> s) -> m s
getAndModify f = do
  s <- get
  modify f
  return s
