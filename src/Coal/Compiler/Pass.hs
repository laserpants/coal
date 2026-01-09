{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass (
  Pass (..),
  BuildUnit (..),
  (>->),
  unitPathName,
  mapPass,
  liftPass,
  overlayEnvironment,
  partitionBuildUnits,
  tickBar,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build
import Coal.Compiler.Build.Core (typeConstructorEnv)
import Coal.Compiler.Environment
import Coal.Compiler.Stack (CompilerT, getCurrentBuildC)
import Coal.Language.Module
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (asks, local)
import Control.Monad.State (evalStateT)
import Data.Foldable (for_)
import Extras (Name)
import System.Console.AsciiProgress

data BuildUnit a
  = BSource a
  | BCached (ModuleBuild Metadata)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

unitPathName :: BuildUnit (Module a k ()) -> Name
unitPathName =
  \case
    BSource m ->
      modulePathName m
    BCached b ->
      principalPath (moduleCurrentPath b)

partitionBuildUnits :: [BuildUnit a] -> ([a], [ModuleBuild Metadata])
partitionBuildUnits = foldr (flip go) ([], [])
 where
  go (sources, cached) =
    \case
      BSource m ->
        (m : sources, cached)
      BCached b ->
        (sources, b : cached)

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
            , compilerCodataAccessorEnvironment = moduleCodataAccessors
            , compilerTraitEnvironment = moduleTraits
            , compilerInstanceEnvironment = moduleInstances
            , compilerDictionaryNameEnvironment = mempty
            , compilerKernelEnvironment = KernelEnvironment mempty mempty mempty
            }
      )
      (runPassAndTickBar p i)
