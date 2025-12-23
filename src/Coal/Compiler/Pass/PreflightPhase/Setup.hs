{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.Setup (passSetup) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Builtin.Definitions (insertBuiltinDefinitions, insertExtraDefinitions)
import Coal.Compiler.Embedded (embeddedPaths)
import Coal.Compiler.Pass (BuildUnit (..), Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language (Kind)
import Coal.Language.Module (Module (..), overModuleDefinitions, principalPath)
import Extras (for)

passSetup :: (Monad m) => Pass a m [BuildUnit (Module Metadata Kind ())] [BuildUnit (Module Metadata Kind ())]
passSetup = Pass{runPass = pass}

pass :: (Monad m) => [BuildUnit (Module Metadata Kind ())] -> CompilerT a m [BuildUnit (Module Metadata Kind ())]
pass modules = pure (for modules (fmap setup))

setup :: Module Metadata Kind () -> Module Metadata Kind ()
setup m@(Module p _ _) = overModuleDefinitions ins m
 where
  ins
    | principalPath p `elem` embeddedPaths =
        insertBuiltinDefinitions
    | otherwise =
        insertExtraDefinitions . insertBuiltinDefinitions
