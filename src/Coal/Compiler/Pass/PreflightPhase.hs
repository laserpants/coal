{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.PreflightPhase (preflightPhase) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Pass (BuildUnit, Pass (..), liftPass, mapPass, (>->))
import Coal.Compiler.Pass.DebugOutput (generateDebugArtifacts)
import Coal.Compiler.Pass.PreflightPhase.DoNotation (passDoNotation)
import Coal.Compiler.Pass.PreflightPhase.ImportsTopRule (passImportsTopRule)
import Coal.Compiler.Pass.PreflightPhase.MainEntrypointRule (passMainEntrypointRule)
import Coal.Compiler.Pass.PreflightPhase.NoDuplicateParamsRule (passNoDuplicateParamsRule)
import Coal.Compiler.Pass.PreflightPhase.Setup (passSetup)
import Coal.Compiler.Pass.PreflightPhase.ShadowingRule (passShadowingRule)
import Coal.Compiler.Pass.PreflightPhase.WhereClauses (passWhereClauses)
import Coal.Language (Kind)
import Coal.Language.Module (Module)
import Control.Monad.IO.Class (MonadIO)

preflightPhase :: (MonadIO m) => Pass Metadata m [BuildUnit (Module Metadata Kind ())] [BuildUnit (Module Metadata Kind ())]
preflightPhase =
  passImportsTopRule
    >-> passSetup
    >-> mapPass passWhereClauses
    >-> passDoNotation
    >-> passShadowingRule
    >-> passMainEntrypointRule
    >-> passNoDuplicateParamsRule
    >-> mapPass (liftPass (generateDebugArtifacts "Preflight"))
