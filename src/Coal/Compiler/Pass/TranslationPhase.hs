module Coal.Compiler.Pass.TranslationPhase (translationPhasePasses) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), (>->))
import Coal.Compiler.Pass.TranslationPhase.CheckPatternAnomalies (passCheckPatternAnomalies)
import Coal.Compiler.Pass.TranslationPhase.CompileMatchExpressions (passCompileMatchExpressions)
import Coal.Compiler.Pass.TranslationPhase.CompileNats (passCompileNats)
import Coal.Compiler.Pass.TranslationPhase.DenormalizeAst (passDenormalizeAst)
import Coal.Compiler.Pass.TranslationPhase.ExpandAsPatterns (passExpandAsPatterns)
import Coal.Compiler.Pass.TranslationPhase.ExpandGuards (passExpandGuards)
import Coal.Compiler.Pass.TranslationPhase.ExpandIntegerLiteralPatterns (passExpandIntegerLiteralPatterns)
import Coal.Compiler.Pass.TranslationPhase.ExpandOrPatterns (passExpandOrPatterns)
import Coal.Compiler.Pass.TranslationPhase.ExpandPatterns (passExpandPatterns)
import Coal.Compiler.Pass.TranslationPhase.ExpandRecordPatterns (passExpandRecordPatterns)
import Coal.Compiler.Pass.TranslationPhase.NormalizeAst (passNormalizeAst)
import Coal.Compiler.Pass.TranslationPhase.Placeholders (passPlaceholders)
import Coal.Language (IndexedType, Kind)
import Coal.Language.Module
import Control.Monad.IO.Class (MonadIO)

translationPhasePasses :: (MonadIO m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
translationPhasePasses =
  passNormalizeAst
    >-> passExpandPatterns
    >-> passExpandGuards
    >-> passExpandOrPatterns
    >-> passCheckPatternAnomalies
    >-> passExpandRecordPatterns
    >-> passExpandAsPatterns
    >-> passExpandIntegerLiteralPatterns
    >-> passCompileMatchExpressions
    >-> passPlaceholders
    >-> passCompileNats
    >-> passDenormalizeAst
