{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass (
  Pass (..),
  (>->),
  mapPass,
  liftPass,
  overlayEnvironment,
  tickBar,
) where

import Coal.AST.Metadata (Metadata (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Build.NameEntry (typeConstructorEntryKind)
import Coal.Compiler.Environment
import Coal.Compiler.Stack
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (asks, local)
import Control.Monad.Trans (lift)
import Data.Foldable (for_)
import System.Console.AsciiProgress

newtype Pass a m i o = Pass {runPass :: i -> CompilerT a m o}

tickBar :: (MonadIO m) => CompilerT a m ()
tickBar = do
  pb <- asks compilerProgressBar
  liftIO (for_ pb tick)

runPassAndTickBar :: (MonadIO m) => Pass a m i b -> i -> CompilerT a m b
runPassAndTickBar p i = do
  tickBar
  runPass p i

(>->) :: (MonadIO m) => Pass a m p q -> Pass a m q r -> Pass a m p r
p1 >-> p2 = Pass{runPass = runPassAndTickBar p1 >=> runPassAndTickBar p2}

mapPass :: (MonadIO m) => Pass a m i o -> Pass a m [i] [o]
mapPass p = Pass{runPass = traverse (runPassAndTickBar p)}

liftPass :: (Monad m) => Pass a m i o -> Pass a m (BuildEnvelope i) (BuildEnvelope o)
liftPass (Pass p) = Pass (traverse p)

overlayEnvironment :: (MonadIO m) => Pass Metadata m a b -> Pass Metadata m a b
overlayEnvironment p = Pass{runPass = pass}
 where
  pass i = do
    Build{..} <- getCurrentBuildC
    local
      ( \env ->
          env
            { compilerTypeConstructorEnvironment =
                Environment.mapEnvironment typeConstructorEntryKind buildTypeConstructors
            , compilerKernelEnvironment =
                KernelEnvironment mempty mempty mempty
            }
      )
      (runPassAndTickBar p i)
