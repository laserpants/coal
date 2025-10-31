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
import Coal.Graphviz.Dot (writeDotFile)
import Coal.Kernel.Language (moduleName)
import qualified Coal.Kernel.Language as Kernel
import Coal.Language
import Coal.Language.Module
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (gets)
import Data.Text (Text)
import Extras (Name)

type KernelModule = Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)

generateDebugArtifacts :: (MonadIO m) => Text -> Pass a m KernelModule KernelModule
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
        writeDotFile (ll <> "_" <> moduleName m) m
    pure m

loweringPhase :: (MonadIO m) => Pass Metadata m [Module Metadata Kind IndexedType] ()
loweringPhase =
  mapPass passKernelTranslate
    >-> mapPass (generateDebugArtifacts "Kernel")
    >-> passKernelCode
    >-> passLLVMOutput
