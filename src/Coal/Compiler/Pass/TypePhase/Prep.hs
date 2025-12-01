{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.TypePhase.Prep (passPrep) where

import Coal.Compiler.Build.Internal (buildEnv, prepareBuild)
import Coal.Compiler.Builtin.Definitions (builtinFunctions)
import Coal.Compiler.Config (CompilerConfig (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language (Kind)
import Coal.Language.Module (Module (..), principalPath)
import Control.Monad (unless)
import Control.Monad.Except (MonadIO, liftIO)
import Control.Monad.State (gets)
import qualified Data.Text.IO as Text

passPrep :: (MonadIO m, Monoid a, Eq a) => Pass a m (Module a Kind ()) (Module a Kind ())
passPrep =
  Pass
    { passName = "Prep"
    , runPass = pass
    }

pass :: (MonadIO m, Monoid a, Eq a) => Module a Kind () -> CompilerT a m (Module a Kind ())
pass m@(Module path _ _) = do
  setCompilerCurrentModuleC path
  CompilerConfig{..} <- gets compilerConfig
  unless configSilent $
    liftIO $
      Text.putStrLn (principalPath path)
  clearAssumptionsC
  clearNameStoreC
  (next, build) <- prepareBuild m
  insertModuleC (principalPath path) build
  env <- buildEnv
  setNamesC env
  insertNamesC builtinFunctions
  pure next
