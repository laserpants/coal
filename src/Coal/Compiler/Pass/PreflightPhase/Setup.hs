{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.Setup (passSetup) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Builtin.Definitions (insertBuiltinDefinitions, insertExtraDefinitions)
import Coal.Compiler.Embedded (embeddedPaths)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language (Kind)
import Coal.Language.Module (Module (..), overModuleDefinitions, principalPath)
import Extras (for)

passSetup :: (Monad m) => Pass a m [Module Metadata Kind ()] [Module Metadata Kind ()]
passSetup = Pass{runPass = pass}

pass :: (Monad m) => [Module Metadata Kind ()] -> CompilerT a m [Module Metadata Kind ()]
pass modules = pure (for modules setup)

setup :: Module Metadata Kind () -> Module Metadata Kind ()
setup m@(Module p _ _) = overModuleDefinitions ins m
 where
  ins
    | principalPath p `elem` embeddedPaths =
        insertBuiltinDefinitions
    | otherwise =
        insertExtraDefinitions . insertBuiltinDefinitions
