{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.TranslationPhase (translationPhase) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), localPassM, mapPass, (>->))
import Coal.Compiler.Pass.DebugOutput (generateDebugArtifacts)
import Coal.Compiler.Pass.TranslationPhase.DenormalizeObjects (passDenormalizeObjects)
import Coal.Compiler.Pass.TranslationPhase.ExpandAsPatterns (passExpandAsPatterns)
import Coal.Compiler.Pass.TranslationPhase.ExpandPatterns (passExpandPatterns)
import Coal.Compiler.Pass.TranslationPhase.MatchExpressions (passMatchExpressions)
import Coal.Compiler.Pass.TranslationPhase.Nats (passCompileNats)
import Coal.Compiler.Pass.TranslationPhase.NormalizeObjects (passNormalizeObjects)
import Coal.Compiler.Pass.TranslationPhase.OrPatterns
import Coal.Compiler.Pass.TranslationPhase.PatternExhaustiveCheck (passPatternExhaustiveCheck)
import Coal.Compiler.Pass.TranslationPhase.Placeholders (passPlaceholders)
import Coal.Compiler.Pass.TranslationPhase.RecordPatterns (passRecordPatterns)
import Coal.Language
import Coal.Language.Module
import Control.Monad.IO.Class (MonadIO)

translationPhasePasses :: (MonadIO m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
translationPhasePasses =
  passNormalizeObjects
    >-> generateDebugArtifacts "NormalizeObjects"
    >-> passExpandPatterns
    >-> passOrPatterns
    >-> passRecordPatterns
    >-> passPatternExhaustiveCheck
    >-> passExpandAsPatterns
    >-> generateDebugArtifacts "Patterns"
    >-> passMatchExpressions
    >-> generateDebugArtifacts "MatchExpressions"
    >-> passPlaceholders
    >-> generateDebugArtifacts "Placeholders"
    >-> passDenormalizeObjects
    >-> generateDebugArtifacts "DenormalizeObjects"
    >-> passCompileNats
    >-> generateDebugArtifacts "CompileNats"

translationPhase :: (MonadIO m) => Pass Metadata m [Module Metadata Kind IndexedType] [Module Metadata Kind IndexedType]
translationPhase = mapPass (localPassM translationPhasePasses)
