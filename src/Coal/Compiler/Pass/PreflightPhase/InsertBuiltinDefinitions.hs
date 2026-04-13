{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.InsertBuiltinDefinitions (
  passInsertBuiltinDefinitions,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Builtin.Definitions (insertBuiltinDefinitions, insertExtraDefinitions)
import Coal.Compiler.Embedded (embeddedPaths)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path
import Extras (for)

passInsertBuiltinDefinitions :: (Monad m) => Pass a m [BuildEnvelope (Module Metadata () ())] [BuildEnvelope (Module Metadata () ())]
passInsertBuiltinDefinitions = Pass{runPass = passImpl}

passImpl :: (Monad m) => [BuildEnvelope (Module Metadata () ())] -> CompilerT a m [BuildEnvelope (Module Metadata () ())]
passImpl modules = pure (for modules (fmap insertModuleBuiltins))

insertModuleBuiltins :: Module Metadata () () -> Module Metadata () ()
insertModuleBuiltins Module{..} =
  Module
    { moduleDefinitions = insertBuiltins moduleDefinitions
    , ..
    }
 where
  insertBuiltins
    | principalPath modulePath `elem` embeddedPaths = insertBuiltinDefinitions
    | otherwise = insertExtraDefinitions . insertBuiltinDefinitions
