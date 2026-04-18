-- +
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Compiler.Pass.PhasePreflight.InsertBuiltinDefinitions

Insert compiler builtin definitions into modules.

This pass injects compiler-provided builtin definitions into modules during
the preflight phase. Builtins include primitive operations, intrinsic
functions, and standard library foundations that are implemented at the
compiler level rather than in source code.

For embedded/standard library modules (like List, String, etc.), this pass
inserts the core builtin definitions. For user modules, it may insert
additional definitions needed for proper compilation.

This ensures that all modules have access to necessary compiler primitives
without requiring explicit imports or manual definitions of these foundational
operations.
-}
module Coal.Compiler.Pass.PhasePreflight.InsertBuiltinDefinitions (
  passInsertBuiltinDefinitions,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Builtin.Definitions (insertBuiltinDefinitions, insertExtraDefinitions)
import Coal.Compiler.Embedded (embeddedPaths)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path (principalPath)
import Extras (for)

{- | Built-in definition insertion pass.

Inject compiler-provided built-in definitions into modules. Insert core built-ins
for embedded standard library modules and additional definitions for user
modules, ensuring all modules have access to necessary compiler primitives.
-}
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
