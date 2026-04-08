{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.Setup (passSetup) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Unit (BuildUnit (..))
import Coal.Compiler.Builtin.Definitions (insertBuiltinDefinitions2, insertExtraDefinitions2)
import Coal.Compiler.Embedded (embeddedPaths)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language (Kind)
import Coal.Language.Module
import Coal.ProtoLanguage.ProtoModule (ModuleExportList (..), ProtoModule (..))
import Extras (for)

passSetup :: (Monad m) => Pass a m [BuildUnit (Module Metadata Kind ())] [BuildUnit (ProtoModule Metadata () ())]
passSetup = Pass{runPass = pass}

pass :: (Monad m) => [BuildUnit (Module Metadata Kind ())] -> CompilerT a m [BuildUnit (ProtoModule Metadata () ())]
pass modules = pure (for modules (fmap setup2))

setup2 :: Module Metadata Kind () -> ProtoModule Metadata () ()
setup2 m = ProtoModule p x (ins ds) -- overModuleDefinitions ins m
 where
  ProtoModule p x ds = toProtoModule [] m
  ins
    | principalPath p `elem` embeddedPaths =
        insertBuiltinDefinitions2
    | otherwise =
        insertExtraDefinitions2 . insertBuiltinDefinitions2

-- setup :: Module Metadata Kind () -> Module metadata Kind ()
-- setup m@(Module p _ _) = overModuleDefinitions ins m
-- where
--  ins
--    | principalPath p `elem` embeddedPaths =
--        insertBuiltinDefinitions
--    | otherwise =
--        insertExtraDefinitions . insertBuiltinDefinitions
