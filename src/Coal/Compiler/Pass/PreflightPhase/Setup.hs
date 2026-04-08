{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.Setup (passSetup) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Unit (BuildUnit (..))
import Coal.Compiler.Builtin.Definitions (insertBuiltinDefinitions, insertExtraDefinitions)
import Coal.Compiler.Embedded (embeddedPaths)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language.Module.Path
import Coal.ProtoLanguage.ProtoModule (ModuleExportList (..), ProtoModule (..))
import Extras (for)

passSetup :: (Monad m) => Pass a m [BuildUnit (ProtoModule Metadata () ())] [BuildUnit (ProtoModule Metadata () ())]
passSetup = Pass{runPass = pass}

pass :: (Monad m) => [BuildUnit (ProtoModule Metadata () ())] -> CompilerT a m [BuildUnit (ProtoModule Metadata () ())]
pass modules = pure (for modules (fmap setup))

setup :: ProtoModule Metadata () () -> ProtoModule Metadata () ()
setup (ProtoModule p x ds) = ProtoModule p x (ins ds) -- overModuleDefinitions ins m
 where
  ins
    | principalPath p `elem` embeddedPaths =
        insertBuiltinDefinitions
    | otherwise =
        insertExtraDefinitions . insertBuiltinDefinitions
