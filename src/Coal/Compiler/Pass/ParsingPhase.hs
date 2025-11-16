{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.ParsingPhase (parsingPhase) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), mapPass, (>->))
import Coal.Compiler.Pass.DebugOutput (generateDebugArtifacts)
import Coal.Compiler.Pass.ParsingPhase.ImportsTopRule (passImportsTopRule)
import Coal.Compiler.Pass.ParsingPhase.Parsing (passParsing)
import Coal.Language
import Coal.Language.Module
import Control.Monad.IO.Class (MonadIO)

parsingPhase :: (MonadIO m) => Pass Metadata m [FilePath] [Module Metadata Kind ()]
parsingPhase =
  passParsing
    >-> passImportsTopRule
    >-> mapPass (generateDebugArtifacts "Parsing")
