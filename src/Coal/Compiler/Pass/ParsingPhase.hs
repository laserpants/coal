{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.ParsingPhase (parsingPhase) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Unit (BuildUnit (..))
import Coal.Compiler.Pass (Pass (..), liftPass, mapPass, (>->))
import Coal.Compiler.Pass.ParsingPhase.CheckDeps (passCheckDeps)
import Coal.Compiler.Pass.ParsingPhase.Parsing (passParsing)
import Coal.Compiler.Pass.ParsingPhase.TopologicalSort (passTopologicalSort)
import Coal.Language (Kind)
import Coal.ProtoLanguage.ProtoModule (ModuleExportList (..), ProtoModule (..))
import Control.Monad.IO.Class (MonadIO)

parsingPhase :: (MonadIO m) => Pass Metadata m [FilePath] [BuildUnit (ProtoModule Metadata () ())]
parsingPhase =
  passParsing
    >-> passTopologicalSort
    >-> passCheckDeps

--    >-> mapPass (liftPass (generateDebugArtifacts "Parsing"))
