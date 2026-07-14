{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Coal.Compiler.Pass.PhaseTypeChecking (phaseTypeChecking) where

import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), (>->))
import Coal.Compiler.Pass.DebugOutput (generateBuildInfo, generateDebugArtifacts)
import Coal.Compiler.Pass.PhaseTypeChecking.ExpandAliases (passExpandAliases)
import Coal.Compiler.Pass.PhaseTypeChecking.ExpandExpressionFolds (passExpandExpressionFolds)
import Coal.Compiler.Pass.PhaseTypeChecking.ExpandFunctionGroups (passExpandFunctionGroups)
import Coal.Compiler.Pass.PhaseTypeChecking.ExpandLambdaMatchExpressions (passExpandLambdaMatchExpressions)
import Coal.Compiler.Pass.PhaseTypeChecking.ExpandTopLevelFolds (passExpandTopLevelFolds)
import Coal.Compiler.Pass.PhaseTypeChecking.KindIndexing (passKindIndexing)
import Coal.Compiler.Pass.PhaseTypeChecking.PrepareBuild (passPrepareBuild)
import Coal.Compiler.Pass.PhaseTypeChecking.ReportTypeErrors (passReportTypeErrors)
import Coal.Compiler.Pass.PhaseTypeChecking.TypeInference (passTypeInference)
import Coal.Language (IndexedType, Kind)
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path (principalPath)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as Text
import System.IO (hFlush, hPutStr, stderr)

putStatus :: String -> IO ()
putStatus msg = do
  hPutStr stderr $ "\r\ESC[2K" ++ msg
  hFlush stderr

tracedTC :: (MonadIO m) => String -> Pass Metadata m (Module a b c) (Module d e f) -> Pass Metadata m (Module a b c) (Module d e f)
tracedTC label p =
  Pass
    { runPass = \m -> do
        liftIO $ putStatus ("[TypeChecking] " ++ label ++ " (" ++ Text.unpack (principalPath (modulePath m)) ++ ") ...")
        runPass p m
    }

phaseTypeChecking :: (MonadIO m) => Pass Metadata m (Module Metadata () ()) (Module Metadata Kind IndexedType)
phaseTypeChecking =
  tracedTC "KindIndexing" passKindIndexing
    >-> generateDebugArtifacts "KindIndexing"
    >-> tracedTC "ExpandFunctionGroups" passExpandFunctionGroups
    >-> generateDebugArtifacts "ExpandFunctionGroups"
    >-> tracedTC "ExpandAliases" passExpandAliases
    >-> generateDebugArtifacts "ExpandAliases"
    >-> tracedTC "PrepareBuild" passPrepareBuild
    >-> generateDebugArtifacts "PrepareBuild"
    >-> generateBuildInfo "PrepareBuild"
    >-> tracedTC "ExpandTopLevelFolds" passExpandTopLevelFolds
    >-> generateDebugArtifacts "ExpandTopLevelFolds"
    >-> tracedTC "ExpandExpressionFolds" passExpandExpressionFolds
    >-> generateDebugArtifacts "ExpandExpressionFolds"
    >-> tracedTC "ExpandLambdaMatchExpressions" passExpandLambdaMatchExpressions
    >-> generateDebugArtifacts "ExpandLambdaMatchExpressions"
    >-> tracedTC "TypeInference" passTypeInference
    >-> generateDebugArtifacts "TypeInference"
    >-> generateBuildInfo "TypeInference"
    >-> tracedTC "ReportTypeErrors" passReportTypeErrors
    >-> generateDebugArtifacts "ReportTypeErrors"
