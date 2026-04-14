module Coal.Compiler.Pass.PreflightPhase (preflightPhase) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Pass (Pass (..), (>->))
import Coal.Compiler.Pass.PreflightPhase.DesugarDoNotation (passDesugarDoNotation)
import Coal.Compiler.Pass.PreflightPhase.DesugarWhereClauses (passDesugarWhereClauses)
import Coal.Compiler.Pass.PreflightPhase.DetectAliasCycles (passDetectAliasCycles)
import Coal.Compiler.Pass.PreflightPhase.DetectDuplicateParams (passDetectDuplicateParams)
import Coal.Compiler.Pass.PreflightPhase.DetectMainEntrypointMissing (passDetectMainEntrypointMissing)
import Coal.Compiler.Pass.PreflightPhase.DetectMisplacedImportStatements (passDetectMisplacedImportStatements)
import Coal.Compiler.Pass.PreflightPhase.DetectShadowing (passDetectShadowing)
import Coal.Compiler.Pass.PreflightPhase.InsertBuiltinDefinitions (passInsertBuiltinDefinitions)
import Coal.Compiler.Pass.PreflightPhase.RefreshCache (passRefreshCache)
import Coal.Compiler.Pass.PreflightPhase.SortModules (passSortModules)
import Coal.Language.Module (Module (..))
import Control.Monad.IO.Class (MonadIO)

preflightPhase :: (MonadIO m) => Pass Metadata m [BuildEnvelope (Module Metadata () ())] [BuildEnvelope (Module Metadata () ())]
preflightPhase =
  passSortModules
    >-> passRefreshCache
    >-> passDetectMisplacedImportStatements
    >-> passInsertBuiltinDefinitions
    >-> passDesugarWhereClauses
    >-> passDesugarDoNotation
    >-> passDetectAliasCycles
    >-> passDetectShadowing
    >-> passDetectMainEntrypointMissing
    >-> passDetectDuplicateParams
