{-# LANGUAGE FlexibleContexts #-}

module Noll.Utils.Control.Monad.Writer (tellLeft, tellRight) where

import Control.Monad.Writer (MonadWriter, tell)

{-# INLINE tellLeft #-}
tellLeft :: (MonadWriter [Either e a] m) => [e] -> m ()
tellLeft = tell . fmap Left

{-# INLINE tellRight #-}
tellRight :: (MonadWriter [Either e a] m) => [a] -> m ()
tellRight = tell . fmap Right
