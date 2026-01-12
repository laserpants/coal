{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.TypePhase (typePhasePasses) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), overlayEnvironment, (>->))
import Coal.Compiler.Pass.DebugOutput (generateDebugArtifacts)
import Coal.Compiler.Pass.TypePhase.Errors (passTypePhaseErrors)
import Coal.Compiler.Pass.TypePhase.ExpandAliases (passExpandAliases)
import Coal.Compiler.Pass.TypePhase.ExpandCotypes (passExpandCotypes)
import Coal.Compiler.Pass.TypePhase.ExpandFunctionGroups (passExpandFunctionGroups)
import Coal.Compiler.Pass.TypePhase.ExpressionFolds (passExpressionFolds)
import Coal.Compiler.Pass.TypePhase.ExpressionUnfolds (passExpressionUnfolds)
import Coal.Compiler.Pass.TypePhase.LambdaMatchExpansion (passLambdaMatchExpansion)
import Coal.Compiler.Pass.TypePhase.Prep (passPrep)
import Coal.Compiler.Pass.TypePhase.TopLevelFolds (passTopLevelFolds)
import Coal.Compiler.Pass.TypePhase.TopLevelUnfolds (passTopLevelUnfolds)
import Coal.Compiler.Pass.TypePhase.TypeInference (passTypeInference)
import Coal.Language (IndexedType, Kind)
import Coal.Language.Module (Module)
import Control.Monad.IO.Class (MonadIO)

typePhasePasses :: (MonadIO m) => Pass Metadata m (Module Metadata Kind ()) (Module Metadata Kind IndexedType)
typePhasePasses =
  passPrep
    >-> passExpandFunctionGroups
    >-> generateDebugArtifacts "IntegerLiteralPatterns"
    >-> overlayEnvironment passExpandAliases
    >-> generateDebugArtifacts "ExpandAliases"
    >-> passTopLevelUnfolds
    >-> passTopLevelFolds
    >-> passExpressionUnfolds
    >-> passExpressionFolds
    >-> generateDebugArtifacts "FoldsUnfolds"
    >-> passLambdaMatchExpansion
    >-> overlayEnvironment passTypeInference
    >-> generateDebugArtifacts "TypeInference"
    >-> passTypePhaseErrors
    >-> passExpandCotypes
    >-> generateDebugArtifacts "ExpandCotypes"
