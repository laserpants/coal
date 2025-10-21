{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.LoweringPhase (loweringPhase) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), mapPass, (>->))
import Coal.Compiler.Pass.LoweringPhase.KernelCode (passKernelCode)
import Coal.Compiler.Pass.LoweringPhase.KernelTranslate (passKernelTranslate)
import Coal.Compiler.Pass.LoweringPhase.LLVMOutput (passLLVMOutput)
import Coal.Graphviz.Dot (Dot (..), writeDotFile)
import Coal.Language
import Coal.Language.Module
import Control.Monad.IO.Class (MonadIO, liftIO)
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
    liftIO $ writeDotFile ll m
    pure m

loweringPhase :: (MonadIO m) => Pass Metadata m [Module Metadata Kind IndexedType] ()
loweringPhase =
  mapPass passKernelTranslate
    >-> mapPass (generateDebugArtifacts "LLVM")
    >-> passKernelCode
    >-> passLLVMOutput
