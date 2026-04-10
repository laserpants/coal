{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.LoweringPhase (loweringPhase) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Unit (BuildUnit (..))
import Coal.Compiler.Config (CompilerConfig (..))
import Coal.Compiler.Pass (Pass (..), liftPass, mapPass, (>->))
import Coal.Compiler.Pass.LoweringPhase.KernelCode (passKernelCode)
import Coal.Compiler.Pass.LoweringPhase.KernelTranslate (passKernelTranslate)
import Coal.Compiler.Pass.LoweringPhase.LLVMOutput (passLLVMOutput)
import Coal.Kernel.Compiler (KernelModule)
import Coal.Kernel.Language (moduleName)
import Coal.Language (IndexedType, Kind)
import Coal.ProtoLanguage.ProtoModule
import Control.Monad (when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (gets)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Extras (Name)

-- generateDebugArtifacts :: (MonadIO m) => Text -> Pass a m KernelModule KernelModule
-- generateDebugArtifacts ll = Pass{runPass = run}
-- where
--  run m = do
--    CompilerConfig{..} <- gets compilerConfig
--    when configGenerateDotFiles $
--      liftIO $
--        writeDotFile (ll <> "_" <> moduleName m) m
--    pure m

loweringPhase :: (MonadIO m, MonadMask m) => Pass Metadata m [BuildUnit (ProtoModule Metadata Kind IndexedType)] [(Name, ByteString)]
loweringPhase =
  mapPass passKernelTranslate
    --    >-> mapPass (liftPass (generateDebugArtifacts "Kernel"))
    >-> passKernelCode
    >-> passLLVMOutput
