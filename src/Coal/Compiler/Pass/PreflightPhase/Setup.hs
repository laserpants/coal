{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.Setup (passSetup) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Builtin.Definitions (insertBuiltinDefinitions, insertExtraDefinitions)
import Coal.Compiler.Embedded (embeddedPaths)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language.Module (Module (..), ModuleExportList (..))
import Coal.Language.Module.Path
import Extras (for)

passSetup :: (Monad m) => Pass a m [BuildEnvelope (Module Metadata () ())] [BuildEnvelope (Module Metadata () ())]
passSetup = Pass{runPass = pass}

pass :: (Monad m) => [BuildEnvelope (Module Metadata () ())] -> CompilerT a m [BuildEnvelope (Module Metadata () ())]
pass modules = pure (for modules (fmap setup))

setup :: Module Metadata () () -> Module Metadata () ()
setup (Module p x ds) = Module p x (ins ds) -- overModuleDefinitions ins m
 where
  ins
    | principalPath p `elem` embeddedPaths =
        insertBuiltinDefinitions
    | otherwise =
        insertExtraDefinitions . insertBuiltinDefinitions
