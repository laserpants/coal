{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.LoweringPhase (loweringPhase) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Compiler.Config (CompilerConfig (..))
import Coal.Compiler.Pass (Pass (..), mapPass, (>->))
import Coal.Compiler.Pass.LoweringPhase.KernelCode (passKernelCode)
import Coal.Compiler.Pass.LoweringPhase.KernelTranslate (passKernelTranslate)
import Coal.Compiler.Pass.LoweringPhase.LLVMOutput (passLLVMOutput)
import Coal.Compiler.Stack
import Coal.Graphviz.Dot (Dot (..), writeDotFile)
import Coal.Language
import Coal.Language.Module
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (gets)
import Data.Text (Text)
import Prettyprinter

generateDebugArtifacts :: (MonadIO m, Pretty t, Dot t o) => Text -> Pass a m o o
generateDebugArtifacts ll =
  Pass
    { passName = "debug<" <> ll <> ">"
    , runPass = run
    }
 where
  run m = do
    CompilerConfig{..} <- gets compilerConfig
    when configGenerateDotFiles $
      liftIO $
        writeDotFile ll m
    pure m

loweringPhase :: (MonadIO m) => Pass Metadata m [Module Metadata Kind IndexedType] ()
loweringPhase =
  mapPass passKernelTranslate
    >-> mapPass (generateDebugArtifacts "LLVM")
    >-> passKernelCode
    >-> passLLVMOutput
