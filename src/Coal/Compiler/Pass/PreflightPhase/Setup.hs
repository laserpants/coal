{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.Setup (passSetup) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Builtin.Definitions (insertBuiltinDefinitions)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language (Kind)
import Coal.Language.Module (Module, overModuleDefinitions)

passSetup :: (Monad m) => Pass a m [Module Metadata Kind ()] [Module Metadata Kind ()]
passSetup =
  Pass
    { passName = "Setup"
    , runPass = pass
    }

pass :: (Monad m) => [Module Metadata Kind ()] -> CompilerT a m [Module Metadata Kind ()]
pass modules = pure (overModuleDefinitions insertBuiltinDefinitions <$> modules)
