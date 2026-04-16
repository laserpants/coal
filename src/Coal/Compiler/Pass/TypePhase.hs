module Coal.Compiler.Pass.TypePhase (typePhasePasses) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), (>->))
import Coal.Compiler.Pass.TypePhase.ExpandAliases (passExpandAliases)
import Coal.Compiler.Pass.TypePhase.ExpandExpressionFolds (passExpandExpressionFolds)
import Coal.Compiler.Pass.TypePhase.ExpandFunctionGroups (passExpandFunctionGroups)
import Coal.Compiler.Pass.TypePhase.ExpandLambdaMatchExpressions (passExpandLambdaMatchExpressions)
import Coal.Compiler.Pass.TypePhase.ExpandTopLevelFolds (passExpandTopLevelFolds)
import Coal.Compiler.Pass.TypePhase.KindIndexing (passKindIndexing)
import Coal.Compiler.Pass.TypePhase.PrepareBuild (passPrepareBuild)
import Coal.Compiler.Pass.TypePhase.ReportTypeErrors (passReportTypeErrors)
import Coal.Compiler.Pass.TypePhase.TypeInference (passTypeInference)
import Coal.Language (IndexedType, Kind)
import Coal.Language.Module
import Control.Monad.IO.Class (MonadIO)

typePhasePasses :: (MonadIO m) => Pass Metadata m (Module Metadata () ()) (Module Metadata Kind IndexedType)
typePhasePasses =
  passKindIndexing
    >-> passExpandFunctionGroups
    >-> passExpandAliases
    >-> passPrepareBuild
    >-> passExpandTopLevelFolds
    >-> passExpandExpressionFolds
    >-> passExpandLambdaMatchExpressions
    >-> passTypeInference
    >-> passReportTypeErrors
