{-# LANGUAGE FlexibleContexts #-}

module Extras.Control.Monad.Writer (tellLeft, tellRight, listenOnly) where

import Control.Monad.Writer (MonadWriter, listen, tell)

{-# INLINE tellLeft #-}
tellLeft :: (MonadWriter [Either e a] m) => [e] -> m ()
tellLeft = tell . fmap Left

{-# INLINE tellRight #-}
tellRight :: (MonadWriter [Either e a] m) => [a] -> m ()
tellRight = tell . fmap Right

{-# INLINE listenOnly #-}
listenOnly :: (MonadWriter w m) => m a -> m w
listenOnly = fmap snd . listen
