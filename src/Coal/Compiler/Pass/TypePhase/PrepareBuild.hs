module Coal.Compiler.Pass.TypePhase.PrepareBuild (passPrepareBuild) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Prep (prepareBuild)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language (Kind)
import Coal.Language.Module (Module (..))
import Control.Monad.Except (MonadIO)

passPrepareBuild :: (MonadIO m) => Pass Metadata m (Module Metadata Kind ()) (Module Metadata Kind ())
passPrepareBuild = Pass{runPass = passImpl}

passImpl :: (MonadIO m) => Module Metadata Kind () -> CompilerT Metadata m (Module Metadata Kind ())
passImpl m = do
  prepareBuild m
  return m
