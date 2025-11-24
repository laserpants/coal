{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.PreflightPhase (preflightPhase) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), mapPass, (>->))
import Coal.Compiler.Pass.DebugOutput (generateDebugArtifacts)
import Coal.Compiler.Pass.PreflightPhase.MainEntrypointRule (passMainEntrypointRule)
import Coal.Compiler.Pass.PreflightPhase.NoDuplicateParamsRule (passNoDuplicateParamsRule)
import Coal.Compiler.Pass.PreflightPhase.Setup (passSetup)
import Coal.Compiler.Pass.PreflightPhase.ShadowingRule (passShadowingRule)
import Coal.Compiler.Pass.PreflightPhase.TopologicalSort (passTopologicalSort)
import Coal.Language (Kind)
import Coal.Language.Module (Module)
import Control.Monad.IO.Class (MonadIO)

preflightPhase :: (MonadIO m) => Pass Metadata m [Module Metadata Kind ()] [Module Metadata Kind ()]
preflightPhase =
  passTopologicalSort
    >-> passSetup
    >-> passShadowingRule
    >-> passMainEntrypointRule
    >-> passNoDuplicateParamsRule
    >-> mapPass (generateDebugArtifacts "Preflight")
