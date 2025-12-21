{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.ParsingPhase (parsingPhase) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), mapPass, (>->))
import Coal.Compiler.Pass.DebugOutput (generateDebugArtifacts)
import Coal.Compiler.Pass.ParsingPhase.Parsing (passParsing)
import Coal.Language (Kind)
import Coal.Language.Module (Module)
import Control.Monad.IO.Class (MonadIO)

parsingPhase :: (MonadIO m) => Pass Metadata m [FilePath] [Module Metadata Kind ()]
parsingPhase = passParsing >-> mapPass (generateDebugArtifacts "Parsing")
