{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass (Pass (..), (>->), mapPass, overlayEnvironment) where

import Coal.Ast.Metadata (Metadata (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Environment
import Coal.Compiler.Module.Builders (typeConstructorEnv)
import Coal.Compiler.Module.Bundle
import Coal.Compiler.Stack (CompilerT)
import Coal.Compiler.State
import Coal.Language (IndexedType, Kind)
import Coal.Language.Module
import Control.Monad ((>=>))
import Control.Monad.Reader (local)
import Control.Monad.State (evalStateT, gets)
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

overlayEnvironment :: (Monad m) => Pass Metadata m (Module Metadata Kind t) (Module Metadata Kind IndexedType) -> Pass Metadata m (Module Metadata Kind t) (Module Metadata Kind IndexedType)
overlayEnvironment p =
  Pass
    { passName = "overlay<" <> passName p <> ">"
    , runPass = pass
    }
 where
  pass m@(Module path _ _) = do
    modules <- gets compilerModules
    case Environment.lookup (principalPath path) modules of
      Nothing ->
        error "Implementation error"
      Just ModuleBundle{..} -> do
        kinds <- evalStateT typeConstructorEnv ModuleBundle{..}
        let env =
              CompilerEnvironment
                { compilerDataConstructorEnvironment = moduleDataConstructors
                , compilerTypeConstructorEnvironment = kinds
                , compilerAliasEnvironment = moduleAliases
                , compilerCodataAccessorEnvironment = moduleCodataAccessors
                , compilerTraitEnvironment = moduleTraits
                , compilerInstanceEnvironment = moduleInstances
                , compilerDictionaryNameEnvironment = mempty
                , compilerKernelEnvironment = KernelEnvironment mempty mempty mempty
                }
        local (const env) (runPass p m)
