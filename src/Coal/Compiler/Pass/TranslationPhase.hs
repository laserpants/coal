{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.TranslationPhase (translationPhasePasses) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), (>->))
import Coal.Compiler.Pass.DebugOutput (generateDebugArtifacts)
import Coal.Compiler.Pass.TranslationPhase.DenormalizeObjects (passDenormalizeObjects)
import Coal.Compiler.Pass.TranslationPhase.ExpandAsPatterns (passExpandAsPatterns)
import Coal.Compiler.Pass.TranslationPhase.ExpandGuards (passExpandGuards)
import Coal.Compiler.Pass.TranslationPhase.ExpandIntegerLiteralPatterns (passExpandIntegerLiteralPatterns)
import Coal.Compiler.Pass.TranslationPhase.ExpandPatterns (passExpandPatterns)
import Coal.Compiler.Pass.TranslationPhase.MatchExpressions (passMatchExpressions)
import Coal.Compiler.Pass.TranslationPhase.Nats (passCompileNats)
import Coal.Compiler.Pass.TranslationPhase.NormalizeObjects (passNormalizeObjects)
import Coal.Compiler.Pass.TranslationPhase.OrPatterns (passOrPatterns)
import Coal.Compiler.Pass.TranslationPhase.PatternExhaustiveCheck (passPatternExhaustiveCheck)
import Coal.Compiler.Pass.TranslationPhase.Placeholders (passPlaceholders)
import Coal.Compiler.Pass.TranslationPhase.RecordPatterns (passRecordPatterns)
import Coal.Language (IndexedType, Kind)
import Coal.Language.Module (Module)
import Coal.ProtoLanguage.ProtoModule
import Control.Monad.IO.Class (MonadIO)

translationPhasePasses :: (MonadIO m) => Pass Metadata m (ProtoModule Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
translationPhasePasses =
    passNormalizeObjects
    >-> generateDebugArtifacts "NormalizeObjects"
    >-> passExpandPatterns
    >-> passExpandGuards
    >-> passOrPatterns
    >-> generateDebugArtifacts "OrPatterns"
    >-> passPatternExhaustiveCheck
    >-> passRecordPatterns
    >-> generateDebugArtifacts "RecordPatterns"
    >-> passExpandAsPatterns
    >-> passExpandIntegerLiteralPatterns
    >-> generateDebugArtifacts "Patterns"
    >-> passMatchExpressions
    >-> generateDebugArtifacts "MatchExpressions"
    >-> passPlaceholders
    >-> generateDebugArtifacts "Placeholders"
    >-> passCompileNats
    >-> generateDebugArtifacts "CompileNats"
    >-> passDenormalizeObjects
    >-> generateDebugArtifacts "DenormalizeObjects"
