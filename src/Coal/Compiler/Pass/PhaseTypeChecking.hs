{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.PhaseTypeChecking (phaseTypeChecking) where

import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), (>->))
import Coal.Compiler.Pass.DebugOutput (generateDebugArtifacts)
import Coal.Compiler.Pass.PhaseTypeChecking.ExpandAliases (passExpandAliases)
import Coal.Compiler.Pass.PhaseTypeChecking.ExpandExpressionFolds (passExpandExpressionFolds)
import Coal.Compiler.Pass.PhaseTypeChecking.ExpandFunctionGroups (passExpandFunctionGroups)
import Coal.Compiler.Pass.PhaseTypeChecking.ExpandLambdaMatchExpressions (passExpandLambdaMatchExpressions)
import Coal.Compiler.Pass.PhaseTypeChecking.ExpandTopLevelFolds (passExpandTopLevelFolds)
import Coal.Compiler.Pass.PhaseTypeChecking.KindIndexing (passKindIndexing)
import Coal.Compiler.Pass.PhaseTypeChecking.PrepareBuild (passPrepareBuild)
import Coal.Compiler.Pass.PhaseTypeChecking.ReportTypeErrors (passReportTypeErrors)
import Coal.Compiler.Pass.PhaseTypeChecking.TypeInference (passTypeInference)
import Coal.Language (IndexedType, Kind)
import Coal.Language.Module (Module)
import Control.Monad.IO.Class (MonadIO)

phaseTypeChecking :: (MonadIO m) => Pass Metadata m (Module Metadata () ()) (Module Metadata Kind IndexedType)
phaseTypeChecking =
  passKindIndexing
    >-> generateDebugArtifacts "KindIndexing"
    >-> passExpandFunctionGroups
    >-> generateDebugArtifacts "ExpandFunctionGroups"
    >-> passExpandAliases
    >-> generateDebugArtifacts "ExpandAliases"
    >-> passPrepareBuild
    >-> generateDebugArtifacts "PrepareBuild"
    >-> passExpandTopLevelFolds
    >-> generateDebugArtifacts "ExpandTopLevelFolds"
    >-> passExpandExpressionFolds
    >-> generateDebugArtifacts "ExpandExpressionFolds"
    >-> passExpandLambdaMatchExpressions
    >-> generateDebugArtifacts "ExpandLambdaMatchExpressions"
    >-> passTypeInference
    >-> generateDebugArtifacts "TypeInference"
    >-> passReportTypeErrors
    >-> generateDebugArtifacts "ReportTypeErrors"
