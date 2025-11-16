{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass (Pass (..), (>->), mapPass, localPass, localPassM) where

import Coal.Ast.Metadata (Metadata (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Environment
import Coal.Compiler.Module.Builders (typeConstructorEnv)
import Coal.Compiler.Module.Bundle -- (ModuleBundle (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Compiler.State
import Coal.Language (IndexedType, Kind)
import Coal.Language.Module
import Control.Monad ((>=>))
import Control.Monad.Reader (ask, local)
import Control.Monad.State (evalStateT, gets)
import Debug.Trace
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

localPass :: (Monad m) => (Module Metadata Kind t -> CompilerEnvironment -> CompilerEnvironment) -> Pass Metadata m (Module Metadata Kind t) (Module Metadata Kind IndexedType) -> Pass Metadata m (Module Metadata Kind t) (Module Metadata Kind IndexedType)
localPass f p =
  Pass
    { passName = "local<" <> passName p <> ">"
    , runPass = \m -> local (f m) (runPass p m)
    }

localPassM :: (Monad m) => Pass Metadata m (Module Metadata Kind t) (Module Metadata Kind IndexedType) -> Pass Metadata m (Module Metadata Kind t) (Module Metadata Kind IndexedType)
localPassM p =
  Pass
    { passName = "localM<" <> passName p <> ">"
    , runPass = pass
    }
 where
  pass m@(Module path _ _) = do
    modules <- gets compilerModules
    currentEnv <- ask

    case Environment.lookup (principalPath path) modules of
      Nothing ->
        error "TODO"
      Just ModuleBundle{..} -> do
        kinds <- evalStateT typeConstructorEnv ModuleBundle{..}
        let env =
              CompilerEnvironment
                { compilerDataConstructorEnvironment = moduleDataConstructors
                , compilerTypeConstructorEnvironment = kinds
                , compilerAliasEnvironment = moduleAliases
                , compilerCodataAccessorEnvironment = moduleCodataAccessors
                , compilerTraitEnvironment = compilerTraitEnvironment currentEnv
                , compilerInstanceEnvironment = compilerInstanceEnvironment currentEnv
                , --
                  compilerFoldEnvironment = mempty
                , compilerUnfoldEnvironment = mempty
                , --
                  compilerDictionaryNameEnvironment = mempty
                , compilerKernelEnvironment = KernelEnvironment mempty mempty mempty
                }

        local (const env) (runPass p m)
