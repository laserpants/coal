{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass (Pass (..), (>->), mapPass, overlayEnvironment) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build
import Coal.Compiler.Build.Internal (typeConstructorEnv)
import Coal.Compiler.Environment
import Coal.Compiler.Stack (CompilerT, getCurrentBuildC)
import Control.Monad ((>=>))
import Control.Monad.Reader (local)
import Control.Monad.State (evalStateT)
import Extras (Name)

data Pass a m i o = Pass
  { passName :: Name
  , runPass :: i -> CompilerT a m o
  }

(>->) :: (Monad m) => Pass a m p q -> Pass a m q r -> Pass a m p r
p1 >-> p2 =
  Pass
    { passName = passName p1 <> " > " <> passName p2
    , runPass = runPass p1 >=> runPass p2
    }

mapPass :: (Monad m) => Pass a m i o -> Pass a m [i] [o]
mapPass p =
  Pass
    { passName = "map<" <> passName p <> ">"
    , runPass = traverse (runPass p)
    }

overlayEnvironment :: (Monad m) => Pass Metadata m a b -> Pass Metadata m a b
overlayEnvironment p =
  Pass
    { passName = "overlay<" <> passName p <> ">"
    , runPass = pass
    }
 where
  pass m = do
    ModuleBuild{..} <- getCurrentBuildC
    typeConstructors <- evalStateT typeConstructorEnv ModuleBuild{..}
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
            }
    local (const env) (runPass p m)
