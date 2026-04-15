module Coal.Compiler.Pass.PhaseParsing (phaseParsing) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Pass.PhaseParsing.Parsing (passParsing)
import Coal.Language.Module (Module (..))
import Control.Monad.IO.Class (MonadIO)

phaseParsing :: (MonadIO m) => Pass Metadata m [FilePath] [BuildEnvelope (Module Metadata () ())]
phaseParsing = passParsing
