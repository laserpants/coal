module Coal.Compiler.Pass.TypePhase.Prep2 (passPrep2) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Prep
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language (Kind)
import Coal.Language.Module (Module (..))
import Control.Monad.Except (MonadIO)

passPrep2 :: (MonadIO m) => Pass Metadata m (Module Metadata Kind ()) (Module Metadata Kind ())
passPrep2 = Pass{runPass = passImpl}

passImpl :: (MonadIO m) => Module Metadata Kind () -> CompilerT Metadata m (Module Metadata Kind ())
passImpl m = do
  prepareBuild m
  return m
