{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.PreflightPhase (preflightPhase) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Pass (Pass (..), liftPass, mapPass, (>->))
import Coal.Compiler.Pass.ParsingPhase.CheckDeps (passCheckDeps)
import Coal.Compiler.Pass.ParsingPhase.TopologicalSort (passTopologicalSort)
import Coal.Compiler.Pass.PreflightPhase.DesugarDoNotation (passDesugarDoNotation)
import Coal.Compiler.Pass.PreflightPhase.DetectAliasCycles (passDetectAliasCycles)
import Coal.Compiler.Pass.PreflightPhase.DetectDuplicateParams (passDetectDuplicateParams)
import Coal.Compiler.Pass.PreflightPhase.DetectMainEntrypointMissing (passDetectMainEntrypointMissing)
import Coal.Compiler.Pass.PreflightPhase.DetectMisplacedImportStatements (passDetectMisplacedImportStatements)
import Coal.Compiler.Pass.PreflightPhase.DetectShadowing (passDetectShadowing)
import Coal.Compiler.Pass.PreflightPhase.Setup (passSetup)
import Coal.Compiler.Pass.PreflightPhase.WhereClauses (passWhereClauses)
import Coal.Language (Kind)
import Coal.Language.Module (Module (..), ModuleExportList (..))
import Control.Monad.IO.Class (MonadIO)

preflightPhase :: (MonadIO m) => Pass Metadata m [BuildEnvelope (Module Metadata () ())] [BuildEnvelope (Module Metadata () ())]
preflightPhase =
  passTopologicalSort
    >-> passCheckDeps
    >-> passDetectMisplacedImportStatements
    >-> passSetup
    --    >-> mapPass passWhereClauses
    >-> passDesugarDoNotation
    >-> passDetectAliasCycles
    --    >-> mapPass (liftPass (generateDebugArtifacts "DesugarDoNotation"))
    >-> passDetectShadowing
    >-> passDetectMainEntrypointMissing
    >-> passDetectDuplicateParams

--    >-> mapPass (liftPass (generateDebugArtifacts "Preflight"))
