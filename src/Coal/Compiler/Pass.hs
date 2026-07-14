{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass (
  Pass (..),
  (>->),
  mapPass,
  liftPass,
) where

import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Stack (CompilerT)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO)

newtype Pass a m i o = Pass {runPass :: i -> CompilerT a m o}

(>->) :: (MonadIO m) => Pass a m p q -> Pass a m q r -> Pass a m p r
p1 >-> p2 = Pass{runPass = runPass p1 >=> runPass p2}

mapPass :: (MonadIO m) => Pass a m i o -> Pass a m [i] [o]
mapPass p = Pass{runPass = traverse (runPass p)}

liftPass :: (Monad m) => Pass a m i o -> Pass a m (BuildEnvelope i) (BuildEnvelope o)
liftPass (Pass p) = Pass{runPass = traverse p}
