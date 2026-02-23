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
import Coal.Compiler.Build
import Coal.Compiler.Build.Core (typeConstructorEnv)
import Coal.Compiler.Build.Unit (BuildUnit (..))
import Coal.Compiler.Environment
import Coal.Compiler.Stack (CompilerT, getCurrentBuildC)
import Coal.ProtoCompiler.ProtoStack
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (asks, local)
import Control.Monad.State (evalStateT)
import Data.Foldable (for_)
import System.Console.AsciiProgress

newtype Pass a m i o = Pass {runPass :: i -> CompilerT a (ProtoCompilerT m a) o}

tickBar :: (MonadIO m) => CompilerT a m ()
tickBar = do
  pb <- asks compilerProgressBar
  liftIO (for_ pb tick)

runPassAndTickBar :: (MonadIO m) => Pass a m i b -> i -> CompilerT a (ProtoCompilerT m a) b
runPassAndTickBar p i = do
  tickBar
  runPass p i

(>->) :: (MonadIO m) => Pass a m p q -> Pass a m q r -> Pass a m p r
p1 >-> p2 = Pass{runPass = runPassAndTickBar p1 >=> runPassAndTickBar p2}

mapPass :: (MonadIO m) => Pass a m i o -> Pass a m [i] [o]
mapPass p = Pass{runPass = traverse (runPassAndTickBar p)}

liftPass :: (Monad m) => Pass a m i o -> Pass a m (BuildUnit i) (BuildUnit o)
liftPass (Pass p) = Pass (traverse p)

overlayEnvironment :: (MonadIO m) => Pass Metadata m a b -> Pass Metadata m a b
overlayEnvironment p = Pass{runPass = pass}
 where
  pass i = do
    ModuleBuild{..} <- getCurrentBuildC
    typeConstructors <- evalStateT typeConstructorEnv ModuleBuild{..}
    local
      ( \env ->
          env
            { compilerDataConstructorEnvironment = moduleDataConstructors
            , compilerTypeConstructorEnvironment = typeConstructors
            , compilerAliasEnvironment = moduleAliases
            , compilerTraitEnvironment = moduleTraits
            , compilerInstanceEnvironment = moduleInstances
            , compilerDictionaryNameEnvironment = mempty
            , compilerKernelEnvironment = KernelEnvironment mempty mempty mempty
            }
      )
      (runPassAndTickBar p i)
