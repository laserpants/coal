{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.ParsingPhase (parsingPhase) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Pass (Pass (..), liftPass, mapPass, (>->))
import Coal.Compiler.Pass.ParsingPhase.CheckDeps (passCheckDeps)
import Coal.Compiler.Pass.ParsingPhase.Parsing (passParsing)
import Coal.Compiler.Pass.ParsingPhase.TopologicalSort (passTopologicalSort)
import Coal.Language (Kind)
import Coal.Language.Module (Module (..), ModuleExportList (..))
import Control.Monad.IO.Class (MonadIO)

parsingPhase :: (MonadIO m) => Pass Metadata m [FilePath] [BuildEnvelope (Module Metadata () ())]
parsingPhase =
  passParsing

--    >-> mapPass (liftPass (generateDebugArtifacts "Parsing"))
