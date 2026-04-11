{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.Setup (passSetup) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Builtin.Definitions (insertBuiltinDefinitions, insertExtraDefinitions)
import Coal.Compiler.Embedded (embeddedPaths)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language.Module.Path
import Coal.ProtoLanguage.ProtoModule (ModuleExportList (..), ProtoModule (..))
import Extras (for)

passSetup :: (Monad m) => Pass a m [BuildEnvelope (ProtoModule Metadata () ())] [BuildEnvelope (ProtoModule Metadata () ())]
passSetup = Pass{runPass = pass}

pass :: (Monad m) => [BuildEnvelope (ProtoModule Metadata () ())] -> CompilerT a m [BuildEnvelope (ProtoModule Metadata () ())]
pass modules = pure (for modules (fmap setup))

setup :: ProtoModule Metadata () () -> ProtoModule Metadata () ()
setup (ProtoModule p x ds) = ProtoModule p x (ins ds) -- overModuleDefinitions ins m
 where
  ins
    | principalPath p `elem` embeddedPaths =
        insertBuiltinDefinitions
    | otherwise =
        insertExtraDefinitions . insertBuiltinDefinitions
