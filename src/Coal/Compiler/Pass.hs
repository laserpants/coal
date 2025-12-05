{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass (Pass (..), (>->), mapPass, overlayEnvironment, tickBar) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build
import Coal.Compiler.Build.Internal (typeConstructorEnv)
import Coal.Compiler.Environment
import Coal.Compiler.Stack (CompilerT, getCurrentBuildC)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (asks, local)
import Control.Monad.State (evalStateT)
import Data.Foldable (for_)
import Extras (Name)
import System.Console.AsciiProgress

data Pass a m i o = Pass
  { passName :: Name
  , runPass :: i -> CompilerT a m o
  }

tickBar :: (MonadIO m) => CompilerT a m ()
tickBar = do
  pb <- asks compilerProgressBar
  for_ pb (liftIO . tick)

runPassAndCount :: (MonadIO m) => Pass a m i b -> i -> CompilerT a m b
runPassAndCount p i = do
  tickBar
  runPass p i

(>->) :: (MonadIO m) => Pass a m p q -> Pass a m q r -> Pass a m p r
p1 >-> p2 =
  Pass
    { passName = passName p1 <> " > " <> passName p2
    , runPass = runPassAndCount p1 >=> runPassAndCount p2
    }

mapPass :: (MonadIO m) => Pass a m i o -> Pass a m [i] [o]
mapPass p =
  Pass
    { passName = "map<" <> passName p <> ">"
    , runPass = traverse (runPassAndCount p)
    }

overlayEnvironment :: (MonadIO m) => Pass Metadata m a b -> Pass Metadata m a b
overlayEnvironment p =
  Pass
    { passName = "overlay<" <> passName p <> ">"
    , runPass = pass
    }
 where
  pass m = do
    ModuleBuild{..} <- getCurrentBuildC
    typeConstructors <- evalStateT typeConstructorEnv ModuleBuild{..}
    bar <- asks compilerProgressBar
    let env =
          CompilerEnvironment
            { compilerDataConstructorEnvironment = moduleDataConstructors
            , compilerTypeConstructorEnvironment = typeConstructors
            , compilerAliasEnvironment = moduleAliases
            , compilerCodataAccessorEnvironment = moduleCodataAccessors
            , compilerTraitEnvironment = moduleTraits
            , compilerInstanceEnvironment = moduleInstances
            , compilerDictionaryNameEnvironment = mempty
            , compilerKernelEnvironment = KernelEnvironment mempty mempty mempty
            , compilerProgressBar = bar
            }
    local (const env) (runPassAndCount p m)
