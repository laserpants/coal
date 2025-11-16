{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.TypePhase (typePhase) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Compiler.Environment (insertEnv)
import Coal.Compiler.Pass (Pass (..), localPass, localPassM, mapPass, (>->))
import Coal.Compiler.Pass.DebugOutput (generateDebugArtifacts)
import Coal.Compiler.Pass.TypePhase.Errors (passTypePhaseErrors)
import Coal.Compiler.Pass.TypePhase.ExpandAliases (passExpandAliases)
import Coal.Compiler.Pass.TypePhase.ExpandFunctionGroups (passExpandFunctionGroups)
import Coal.Compiler.Pass.TypePhase.ExpandIntegerLiteralPatterns (passExpandIntegerLiteralPatterns)
import Coal.Compiler.Pass.TypePhase.ExpressionFolds (passExpressionFolds)
import Coal.Compiler.Pass.TypePhase.ExpressionUnfolds (passExpressionUnfolds)
import Coal.Compiler.Pass.TypePhase.LambdaMatchExpansion (passLambdaMatchExpansion)
import Coal.Compiler.Pass.TypePhase.TopLevelFolds (passTopLevelFolds)
import Coal.Compiler.Pass.TypePhase.TopLevelUnfolds (passTopLevelUnfolds)
import Coal.Compiler.Pass.TypePhase.TypeInference (passTypeInference)
import Coal.Language
import Coal.Language.Module
import Control.Monad.IO.Class (MonadIO)

typePhasePasses :: (MonadIO m) => Pass Metadata m (Module Metadata Kind ()) (Module Metadata Kind IndexedType)
typePhasePasses =
  passExpandFunctionGroups
    >-> passExpandIntegerLiteralPatterns
    >-> generateDebugArtifacts "IntegerLiteralPatterns"
    >-> passExpandAliases
    >-> passTopLevelUnfolds
    >-> passTopLevelFolds
    >-> passExpressionUnfolds
    >-> passExpressionFolds
    >-> generateDebugArtifacts "FoldsUnfolds"
    >-> passLambdaMatchExpansion
    >-> passTypeInference
    >-> generateDebugArtifacts "TypeInference"
    >-> passTypePhaseErrors

typePhase :: (MonadIO m) => Pass Metadata m [Module Metadata Kind ()] [Module Metadata Kind IndexedType]
typePhase = mapPass (localPass insertEnv (localPassM typePhasePasses))
