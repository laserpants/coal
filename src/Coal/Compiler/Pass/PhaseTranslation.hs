module Coal.Compiler.Pass.PhaseTranslation (phaseTranslation) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), (>->))
import Coal.Compiler.Pass.PhaseTranslation.CheckPatternAnomalies (passCheckPatternAnomalies)
import Coal.Compiler.Pass.PhaseTranslation.CompileMatchExpressions (passCompileMatchExpressions)
import Coal.Compiler.Pass.PhaseTranslation.CompileNats (passCompileNats)
import Coal.Compiler.Pass.PhaseTranslation.DenormalizeAST (passDenormalizeAST)
import Coal.Compiler.Pass.PhaseTranslation.DesugarPatterns (passDesugarPatterns)
import Coal.Compiler.Pass.PhaseTranslation.ExpandAsPatterns (passExpandAsPatterns)
import Coal.Compiler.Pass.PhaseTranslation.ExpandGuards (passExpandGuards)
import Coal.Compiler.Pass.PhaseTranslation.ExpandIntegerLiteralPatterns (passExpandIntegerLiteralPatterns)
import Coal.Compiler.Pass.PhaseTranslation.ExpandOrPatterns (passExpandOrPatterns)
import Coal.Compiler.Pass.PhaseTranslation.ExpandRecordPatterns (passExpandRecordPatterns)
import Coal.Compiler.Pass.PhaseTranslation.NormalizeAST (passNormalizeAST)
import Coal.Compiler.Pass.PhaseTranslation.Placeholders (passPlaceholders)
import Coal.Language (IndexedType, Kind)
import Coal.Language.Module
import Control.Monad.IO.Class (MonadIO)

phaseTranslation :: (MonadIO m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
phaseTranslation =
  passNormalizeAST
    >-> passDesugarPatterns
    >-> passExpandGuards
    >-> passExpandOrPatterns
    >-> passCheckPatternAnomalies
    >-> passExpandRecordPatterns
    >-> passExpandAsPatterns
    >-> passExpandIntegerLiteralPatterns
    >-> passCompileMatchExpressions
    >-> passPlaceholders
    >-> passCompileNats
    >-> passDenormalizeAST
