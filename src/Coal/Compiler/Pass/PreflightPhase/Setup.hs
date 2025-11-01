{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.Setup (passSetup) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Compiler.Builtin.Definitions (insertBuiltinDefinitions)
import Coal.Compiler.Pass
import Coal.Compiler.Stack (CompilerT)
import Coal.Language
import Coal.Language.Module

passSetup :: (Monad m) => Pass a m [Module Metadata Kind ()] [Module Metadata Kind ()]
passSetup =
  Pass
    { passName = "Setup"
    , runPass = pass
    }

pass :: (Monad m) => [Module Metadata Kind ()] -> CompilerT a m [Module Metadata Kind ()]
pass modules = pure (overModuleDefinitions insertBuiltinDefinitions <$> modules)
