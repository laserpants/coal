module Coal.Compiler.Pass.PhasePreflight (phasePreflight) where

import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), (>->))
import Coal.Compiler.Pass.PhasePreflight.DesugarDoNotation (passDesugarDoNotation)
import Coal.Compiler.Pass.PhasePreflight.DesugarWhereClauses (passDesugarWhereClauses)
import Coal.Compiler.Pass.PhasePreflight.DetectAliasCycles (passDetectAliasCycles)
import Coal.Compiler.Pass.PhasePreflight.DetectDuplicateParams (passDetectDuplicateParams)
import Coal.Compiler.Pass.PhasePreflight.DetectInvalidExports (passDetectInvalidExports)
import Coal.Compiler.Pass.PhasePreflight.DetectMainEntrypointMissing (passDetectMainEntrypointMissing)
import Coal.Compiler.Pass.PhasePreflight.DetectMisplacedImportStatements (passDetectMisplacedImportStatements)
import Coal.Compiler.Pass.PhasePreflight.DetectShadowing (passDetectShadowing)
import Coal.Compiler.Pass.PhasePreflight.InsertBuiltinDefinitions (passInsertBuiltinDefinitions)
import Coal.Compiler.Pass.PhasePreflight.RefreshCache (passRefreshCache)
import Coal.Compiler.Pass.PhasePreflight.SortModules (passSortModules)
import Coal.Language.Module (Module (..))
import Control.Monad.IO.Class (MonadIO)

phasePreflight :: (MonadIO m) => Pass Metadata m [BuildEnvelope (Module Metadata () ())] [BuildEnvelope (Module Metadata () ())]
phasePreflight =
  passSortModules
    >-> passRefreshCache
    >-> passDetectMisplacedImportStatements
    >-> passInsertBuiltinDefinitions
    >-> passDesugarWhereClauses
    >-> passDesugarDoNotation
    >-> passDetectAliasCycles
    >-> passDetectShadowing
    >-> passDetectDuplicateParams
    >-> passDetectInvalidExports
    >-> passDetectMainEntrypointMissing
