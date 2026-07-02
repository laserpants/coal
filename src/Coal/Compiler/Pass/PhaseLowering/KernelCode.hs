{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.PhaseLowering.KernelCode (
  passKernelCode,
  compileEnvelopes,
) where

import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT, updateBuildC)
import Coal.LegacyKernel.Builtin.Objects (builtinObjects)
import Coal.LegacyKernel.Compiler (KernelExpr, compile, compileClosureCode)
import Coal.LegacyKernel.Compiler.EntryPoint (entryPoint)
import Coal.LegacyKernel.Compiler.Pass (transformInterpreter)
import Coal.LegacyKernel.Compiler.Pipeline
import Coal.LegacyKernel.Compiler.Pipeline.State (PipelineState (..))
import Coal.LegacyKernel.LLVM
import Coal.LegacyKernel.Language
import qualified Coal.LegacyKernel.Language as Kernel
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (gets)
import Control.Monad.Trans (lift)
import qualified Data.Text as Text
import Extras (Name, forM, isConstructor, (<$$>))

passKernelCode :: (MonadIO m) => Pass Metadata m [BuildEnvelope (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))] [BuildEnvelope (Name, [IRConstruct [IRLine]])]
passKernelCode = Pass{runPass = passImpl}

passImpl :: (MonadIO m) => [BuildEnvelope (Module Type Name (Expr Type))] -> CompilerT Metadata m [BuildEnvelope (Name, [IRConstruct [IRLine]])]
passImpl modules = evalPipelineT (compileEnvelopes (builtin : modules))

compileEnvelopes :: (MonadIO m) => [BuildEnvelope (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))] -> PipelineT (CompilerT Metadata m) [BuildEnvelope (Name, [IRConstruct [IRLine]])]
compileEnvelopes units = do
  mods <- forM units $
    \case
      BSource Module{moduleName, moduleImports, moduleObjects} -> do
        pipelineReset

        names <- collectNames moduleImports
        ctors <- collectConstructors moduleImports

        ir <- compileModule ctors Module{moduleImports = names, ..}
        IRInterpreterEnv{..} <- gets pipelineInterpreterEnv

        let kernelNames = objectSignature <$> moduleObjects
            irTypes = irTypeOf <$$> Environment.toList irInterpreterValueEnv
            kernelConstructors = objectConstructorInfo =<< moduleObjects

        pipelineInsertNames kernelNames
        pipelineInsertIRTypes irTypes
        pipelineInsertConstructors kernelConstructors

        let out = ir <> [entryPoint | moduleName == "Main"]

        lift $
          unless (moduleName == "Builtin$") $ do
            updateBuildC moduleName $
              \Build{..} ->
                pure
                  Build
                    { buildKernelNames = Environment.fromList kernelNames
                    , buildKernelIRTypes = Environment.fromList irTypes
                    , buildKernelConstructors = Environment.fromList kernelConstructors
                    , ..
                    }

        pure (BSource (moduleName, out))
      BCached Build{..} -> do
        pipelineInsertNames (Environment.toList buildKernelNames)
        pipelineInsertIRTypes (Environment.toList buildKernelIRTypes)
        pipelineInsertConstructors (Environment.toList buildKernelConstructors)
        pure (BCached Build{..})

  cc <- transformInterpreter compileClosureCode
  -- TODO: cache?
  pure (BSource ("closures", cc) : mods)
 where
  collectNames :: (MonadIO m) => [Name] -> PipelineT m [(Name, Type)]
  collectNames names = gets (Environment.lookupAll names . Environment.filterNames notConstructor . pipelineNames)

  notConstructor :: Name -> Bool
  notConstructor name =
    let parts = Text.splitOn "." name
     in not (isConstructor (last parts))

  collectConstructors :: (MonadIO m) => [Name] -> PipelineT m [(Name, Int)]
  collectConstructors names = gets (Environment.lookupAll names . pipelineConstructors)

  objectSignature :: Object Type KernelExpr -> (Name, Type)
  objectSignature obj = (objectName obj, typeOf obj)

  compileModule :: (MonadIO m) => [(Name, Int)] -> Module Type (Name, Type) KernelExpr -> PipelineT m [IRConstruct [IRLine]]
  compileModule ctors Module{..} = do
    let external = uncurry OExternal <$> moduleImports
    compile (Environment.fromList ctors) (external <> moduleObjects)
    gets pipelineCode

-- TODO: cache?
builtin :: BuildEnvelope (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))
builtin = BSource builtinObjects
