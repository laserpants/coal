{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.PhaseTranslation (phaseTranslation) where

import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), (>->))
import Coal.Compiler.Pass.DebugOutput (generateBuildInfo, generateDebugArtifacts)
import Coal.Compiler.Pass.PhaseTranslation.CheckPatternAnomalies (passCheckPatternAnomalies)
import Coal.Compiler.Pass.PhaseTranslation.CheckTraitAnnotations (passCheckTraitAnnotations)
import Coal.Compiler.Pass.PhaseTranslation.CompileMatchExpressions (passCompileMatchExpressions)
import Coal.Compiler.Pass.PhaseTranslation.CompileNats (passCompileNats)
import Coal.Compiler.Pass.PhaseTranslation.DenormalizeAST (passDenormalizeAST)
import Coal.Compiler.Pass.PhaseTranslation.DesugarPatterns (passDesugarPatterns)
import Coal.Compiler.Pass.PhaseTranslation.DetectCallCycles (passDetectCallCycles)
import Coal.Compiler.Pass.PhaseTranslation.ExpandAsPatterns (passExpandAsPatterns)
import Coal.Compiler.Pass.PhaseTranslation.ExpandGuards (passExpandGuards)
import Coal.Compiler.Pass.PhaseTranslation.ExpandIntegerLiteralPatterns (passExpandIntegerLiteralPatterns)
import Coal.Compiler.Pass.PhaseTranslation.ExpandOrPatterns (passExpandOrPatterns)
import Coal.Compiler.Pass.PhaseTranslation.ExpandRecordPatterns (passExpandRecordPatterns)
import Coal.Compiler.Pass.PhaseTranslation.InsertDictionaries (passInsertDictionaries)
import Coal.Compiler.Pass.PhaseTranslation.NormalizeAST (passNormalizeAST)
import Coal.Language (IndexedType, Kind)
import Coal.Language.Module (Module)
import Control.Monad.IO.Class (MonadIO)

phaseTranslation :: (MonadIO m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
phaseTranslation =
  passNormalizeAST
    >-> generateDebugArtifacts "NormalizeAST"
    >-> passDesugarPatterns
    >-> generateDebugArtifacts "DesugarPatterns"
    >-> passExpandGuards
    >-> generateDebugArtifacts "ExpandGuards"
    >-> passExpandOrPatterns
    >-> generateDebugArtifacts "ExpandOrPatterns"
    >-> passCheckPatternAnomalies
    >-> generateDebugArtifacts "CheckPatternAnomalies"
    >-> passExpandRecordPatterns
    >-> generateDebugArtifacts "ExpandRecordPatterns"
    >-> passExpandAsPatterns
    >-> generateDebugArtifacts "ExpandAsPatterns"
    >-> passExpandIntegerLiteralPatterns
    >-> generateDebugArtifacts "ExpandIntegerLiteralPatterns"
    >-> passCompileMatchExpressions
    >-> generateDebugArtifacts "CompileMatchExpressions"
    >-> passInsertDictionaries
    >-> generateDebugArtifacts "InsertDictionaries"
    >-> generateBuildInfo "InsertDictionaries"
    >-> passCompileNats
    >-> generateDebugArtifacts "CompileNats"
--    >-> passDetectCallCycles
    >-> generateDebugArtifacts "DetectCallCycles"
    >-> passDenormalizeAST
    >-> generateDebugArtifacts "DenormalizeAST"
    >-> passCheckTraitAnnotations
