{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.PhaseLowering (phaseLowering) where

import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Config (CompilerConfig (..))
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), liftPass, mapPass, (>->))
import Coal.Compiler.Pass.DebugOutput (writeDotFile)
import Coal.Compiler.Pass.PhaseLowering.KernelCode (passKernelCode)
import Coal.Compiler.Pass.PhaseLowering.KernelTranslate (passKernelTranslate)
import Coal.Compiler.Pass.PhaseLowering.LLVMOutput (passLLVMOutput)
import Coal.Compiler.State (CompilerState (..))
import Coal.Kernel.Compiler (KernelModule)
import Coal.Kernel.Language (moduleName)
import Coal.Language (IndexedType, Kind)
import Coal.Language.Module
import Control.Monad (when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (gets)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Extras (Name)

generateDebugArtifacts :: (MonadIO m) => Text -> Pass a m KernelModule KernelModule
generateDebugArtifacts ll = Pass{runPass = run}
 where
  run m = do
    CompilerConfig{..} <- gets compilerConfig
    when configGenerateDebugArtifacts $
      liftIO $
        writeDotFile (ll <> "_" <> moduleName m) m
    pure m

phaseLowering :: (MonadIO m, MonadMask m) => Pass Metadata m [BuildEnvelope (Module Metadata Kind IndexedType)] [(Name, ByteString)]
phaseLowering =
  mapPass passKernelTranslate
    >-> mapPass (liftPass (generateDebugArtifacts "Kernel"))
    >-> passKernelCode
    >-> passLLVMOutput
