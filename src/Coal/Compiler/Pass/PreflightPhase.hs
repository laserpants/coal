{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.PreflightPhase (preflightPhase) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Pass (Pass (..), liftPass, mapPass, (>->))
import Coal.Compiler.Pass.ParsingPhase.CheckDeps (passCheckDeps)
import Coal.Compiler.Pass.ParsingPhase.TopologicalSort (passTopologicalSort)
import Coal.Compiler.Pass.PreflightPhase.DetectAliasCycles (passDetectAliasCycles)
import Coal.Compiler.Pass.PreflightPhase.DoNotation (passDoNotation)
import Coal.Compiler.Pass.PreflightPhase.ImportsTopRule (passImportsTopRule)
import Coal.Compiler.Pass.PreflightPhase.MainEntrypointRule (passMainEntrypointRule)
import Coal.Compiler.Pass.PreflightPhase.NoDuplicateParamsRule (passNoDuplicateParamsRule)
import Coal.Compiler.Pass.PreflightPhase.Setup (passSetup)
import Coal.Compiler.Pass.PreflightPhase.ShadowingRule (passShadowingRule)
import Coal.Compiler.Pass.PreflightPhase.WhereClauses (passWhereClauses)
import Coal.Language (Kind)
import Coal.Language.Module (Module (..), ModuleExportList (..))
import Control.Monad.IO.Class (MonadIO)

preflightPhase :: (MonadIO m) => Pass Metadata m [BuildEnvelope (Module Metadata () ())] [BuildEnvelope (Module Metadata () ())]
preflightPhase =
  passTopologicalSort
    >-> passCheckDeps
    >-> passImportsTopRule
    >-> passSetup
    --    >-> mapPass passWhereClauses
    >-> passDoNotation
    >-> passDetectAliasCycles
    --    >-> mapPass (liftPass (generateDebugArtifacts "DoNotation"))
    >-> passShadowingRule
    >-> passMainEntrypointRule
    >-> passNoDuplicateParamsRule

--    >-> mapPass (liftPass (generateDebugArtifacts "Preflight"))
