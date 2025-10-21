{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.PreflightPhase (preflightPhase) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), mapPass, (>->))
import Coal.Compiler.Pass.DebugOutput (generateDebugArtifacts)
import Coal.Compiler.Pass.PreflightPhase.Setup (passSetup)
import Coal.Compiler.Pass.PreflightPhase.ShadowingRule (passShadowingRule)
import Coal.Compiler.Pass.PreflightPhase.TopologicalSort (passTopologicalSort)
import Coal.Compiler.Pass.PreflightPhase.TypeDefinitions (passTypeDefinitions)
import Coal.Compiler.Pass.PreflightPhase.TypeImports (passTypeImports)
import Coal.Language
import Coal.Language.Module
import Control.Monad.IO.Class (MonadIO)

preflightPhase :: (MonadIO m) => Pass Metadata m [Module Metadata Kind ()] [Module Metadata Kind ()]
preflightPhase =
  passTopologicalSort
    >-> passSetup
    >-> passShadowingRule
    >-> mapPass (passTypeImports >-> passTypeDefinitions)
    >-> mapPass (generateDebugArtifacts "Preflight")
