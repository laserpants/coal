{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.TypePhase.Prep (passPrep) where

import Coal.Compiler.Build.Internal (buildEnv, prepareBuild)
import Coal.Compiler.Builtin.Definitions (builtinFunctions)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language (Kind)
import Coal.Language.Module (Module (..), principalPath)
import Control.Monad.Except (MonadIO)

passPrep :: (MonadIO m, Monoid a, Eq a) => Pass a m (Module a Kind ()) (Module a Kind ())
passPrep =
  Pass
    { passName = "Prep"
    , runPass = pass
    }

pass :: (MonadIO m, Monoid a, Eq a) => Module a Kind () -> CompilerT a m (Module a Kind ())
pass m@(Module path _ _) = do
  setCompilerCurrentModuleC path
  clearAssumptionsC
  clearNameStoreC
  (next, build) <- prepareBuild m
  insertModuleC (principalPath path) build
  env <- buildEnv
  setNamesC env
  insertNamesC builtinFunctions
  pure next
