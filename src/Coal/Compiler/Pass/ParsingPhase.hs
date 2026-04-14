module Coal.Compiler.Pass.ParsingPhase (parsingPhase) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Pass.ParsingPhase.Parsing (passParsing)
import Coal.Language.Module (Module (..))
import Control.Monad.IO.Class (MonadIO)

parsingPhase :: (MonadIO m) => Pass Metadata m [FilePath] [BuildEnvelope (Module Metadata () ())]
parsingPhase = passParsing
